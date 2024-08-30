#include <bitset>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <list>
#include <memory_resource>
#include <queue>
#include <random>
#include <ranges>
#include <sstream>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

using rule = bool (*)();
using name_t = std::int32_t;

class atom {};
class hyperlink;
class normal_atom;
class atom_store;

enum atom_flag : uint8_t {
  Delete = 1 << 0,
};

constexpr std::string_view int2str(name_t n);

struct link {
  ::atom *atom;
  int64_t port;

  [[nodiscard]] auto is_normal() const -> bool { return port >= 0; }
  [[nodiscard]] auto is_hyperlink() const -> bool { return port < 0; }
  [[nodiscard]] auto get_port() const -> size_t {
    return static_cast<size_t>(port);
  }
  [[nodiscard]] auto get_normal() const -> normal_atom * {
    return is_normal() ? reinterpret_cast<normal_atom *>(atom) : nullptr;
  }
  [[nodiscard]] auto get_hyperlink() const -> hyperlink * {
    return is_hyperlink() ? reinterpret_cast<hyperlink *>(atom) : nullptr;
  }
  [[nodiscard]] auto is_int() const -> bool;
  [[nodiscard]] auto get_int() const -> int64_t;
  void set_int(int64_t value);
  [[nodiscard]] auto is_float() const -> bool;
  [[nodiscard]] auto get_float() const -> double;
  void set_float(double value);
};

class hyperlink final : public atom {
public:
  explicit hyperlink(int32_t const label) : label(label) {}
  auto add(normal_atom *atom, size_t pos) -> void;
  auto remove(normal_atom *atom, size_t pos) -> void;
  [[nodiscard]] auto get_arity() const -> size_t { return atoms.size(); }
  [[nodiscard]] auto get_label() const -> name_t { return label; }

private:
  name_t label{};
  std::unordered_set<normal_atom *> atoms;
};

class normal_atom final : public atom {
 public:
  explicit normal_atom(int32_t const label) : label_(label) {}

  [[nodiscard]] auto at(size_t const pos) const -> link { return bonds_[pos]; }
  auto link_with(size_t const pos, normal_atom *atom, size_t const atom_pos)
      -> void {
    bonds_[pos].atom = atom;
    bonds_[pos].port = static_cast<int64_t>(atom_pos);
  }
  auto link_with(size_t const pos, hyperlink *hlink) -> void {
    bonds_[pos].atom = hlink;
    bonds_[pos].port = -1;
  }
  auto remove_at(size_t const pos) -> void {
    auto *atom = bonds_[pos].get_normal();
    atom->set_remove();
    bonds_[pos].atom = nullptr;
  }
  [[nodiscard]] auto get_arity() const -> size_t { return arity_; }
  [[nodiscard]] auto is_plain() const -> bool {
    return data_.index() == PlainIndex;
  }
  [[nodiscard]] auto is_int() const -> bool {
    return data_.index() == IntIndex;
  }
  [[nodiscard]] auto get_int() const -> int64_t {
    return std::get<int64_t>(data_);
  }
  void set_int(int64_t value) { data_ = value; }
  [[nodiscard]] auto is_float() const -> bool {
    return data_.index() == FloatIndex;
  }
  [[nodiscard]] auto get_float() const -> double {
    return std::get<double>(data_);
  }
  void set_float(double value) { data_ = value; }
  void set_remove() {
    flag_ = static_cast<atom_flag>(flag_ | atom_flag::Delete);
  }
  void restore_remove() {
    flag_ = static_cast<atom_flag>(flag_ & ~atom_flag::Delete);
  }
  [[nodiscard]] auto is_removed() const -> bool {
    return (flag_ & atom_flag::Delete) != 0;
  }
  [[nodiscard]] auto get_hlink_at_port(size_t pos) const -> hyperlink *;

  [[nodiscard]] auto to_string() const -> std::string {
    switch (data_.index()) {
      case IntIndex:
        return ::std::to_string(get_int());
      case FloatIndex:
        return ::std::to_string(get_float());
      default:
        return std::string{int2str(label_)};
    }
  }

  [[nodiscard]] auto get_label() const -> name_t { return label_; }

 private:
  friend hyperlink;
  friend atom_store;

  using data_type = std::variant<std::monostate, int64_t, double>;
  constexpr static size_t PlainIndex = 0;
  constexpr static size_t IntIndex = 1;
  constexpr static size_t FloatIndex = 2;

  name_t label_{};
  uint32_t arity_;
  atom_flag flag_{};
  data_type data_{std::monostate{}};
  // std::vector<link> bonds{};
  link bonds_[1];

  void clone_to(normal_atom *atom) const {
    atom->flag_ = flag_;
    std::copy(bonds_, bonds_ + arity_, atom->bonds_);
    atom->data_ = data_;
  }
};

auto link::is_int() const -> bool {
  return is_normal() and get_normal()->is_int();
}
auto link::get_int() const -> int64_t { return get_normal()->get_int(); }
void link::set_int(int64_t value) { get_normal()->set_int(value); }
auto link::is_float() const -> bool {
  return is_normal() and get_normal()->is_float();
}
auto link::get_float() const -> double { return get_normal()->get_float(); }
void link::set_float(double value) { get_normal()->set_float(value); }

auto hyperlink::add(normal_atom *atom, size_t const pos) -> void {
  atoms.insert(atom);
  atom->bonds_[pos].atom = this;
  atom->bonds_[pos].port = -1;
}

auto hyperlink::remove(normal_atom *atom, size_t const pos) -> void {
  atoms.erase(atom);
  atom->bonds_[pos].atom = nullptr;
}

auto get_hlink_at_port(const normal_atom *atom, const size_t pos)
    -> hyperlink * {
  return atom->at(pos).get_hyperlink();
}

class atom_store {
 public:
  atom_store() = default;
  atom_store(std::initializer_list<size_t> sizes) {
    for (auto size : sizes) {
      atoms_by_arity_[size] = std::vector<normal_atom *>{};
      atoms_by_arity_[size].reserve(32);
      remove_queue_[size] = {};
    }
  }

  auto create_atom(name_t const name, size_t const bond_count)
      -> normal_atom * {
    if (auto it = remove_queue_.find(bond_count);
        it != remove_queue_.end() and !it->second.empty()) {
      auto &q = it->second;
      auto *a = q.top();
      q.pop();
      a->label_ = name;
      a->restore_remove();
      return a;
    }
    // allocate memory for the atom
    void *mem =
        pool_.allocate(sizeof(normal_atom) + (bond_count - 1) * sizeof(link));
    auto *a = new (mem) normal_atom{name};
    a->arity_ = bond_count;

    if (not atoms_by_arity_.contains(bond_count)) [[unlikely]] {
      auto vec = std::vector<normal_atom *>{};
      vec.reserve(16);
      atoms_by_arity_.emplace(bond_count, std::move(vec));
    }

    atoms_by_arity_.at(bond_count).push_back(a);
    return a;
  }

  auto clone_atom(normal_atom const *atom, size_t const pos) -> normal_atom * {
    auto *const nr = atom->at(pos).get_normal();
    auto *a = create_atom(nr->label_, nr->get_arity());
    nr->clone_to(a);
    return a;
  }

  auto create_hyperlink(name_t const name) -> hyperlink * {
    auto *hl = new hyperlink{name};
    hyperlinks_.insert(hl);
    return hl;
  }

  void remove_atom(normal_atom *atom) {
    atom->set_remove();
    if (not remove_queue_.contains(atom->arity_)) {
      remove_queue_[atom->arity_] = {};
    }
    remove_queue_[atom->arity_].push(atom);
  }

  auto find_atom(name_t const name, int const arity) {
    return atoms_by_arity_[arity] | std::views::filter([name](auto *atom) {
             return atom->label_ == name and not atom->is_removed();
           });
  }

  [[nodiscard]] auto dump() const -> std::string {
    std::stringstream res;
    std::unordered_set<atom const *> visited;
    for (const auto &atoms : atoms_by_arity_ | std::views::values) {
      for (auto const *a : atoms) {
        if (not a->is_plain() or a->is_removed() or visited.contains(a)) {
          continue;
        }
        dfs_dump(a, res, visited);
      }
    }
    return res.str();
  }

 private:
  std::unordered_map<uint32_t, std::vector<normal_atom *>> atoms_by_arity_{};
  std::unordered_map<uint32_t, std::stack<normal_atom *>> remove_queue_{};
  std::unordered_set<hyperlink *> hyperlinks_{};

  std::pmr::unsynchronized_pool_resource pool_{};

  static auto dfs_dump(normal_atom const *cur, std::stringstream &ss,
                       std::unordered_set<atom const *> &visited) -> void {
    visited.insert(cur);
    ss << cur->to_string() << "(";
    size_t count = 0;
    for (auto i = 0; i < cur->arity_; ++i) {
      auto link = cur->at(i);
      if (link.atom != nullptr and !visited.contains(link.atom)) {
        // check if the atom is a hyperlink
        if (const auto *na = link.get_normal()) {
          dfs_dump(na, ss, visited);
          ss << ",";
          count++;
        } else if (const auto *hl = link.get_hyperlink()) {
          ss << hl->get_label() << ",";
          count++;
        }
      }
    }
    ss.seekp(-1, std::ios_base::end);
    if (count != 0) {
      ss << ")";
    }
  }
};

atom_store global_atoms{};

auto create_atom(const name_t name, const size_t bond_count) -> normal_atom * {
  return global_atoms.create_atom(name, bond_count);
}

auto clone_atom(const normal_atom *atom, const size_t pos) -> normal_atom * {
  return global_atoms.clone_atom(atom, pos);
}

auto create_hyperlink(const name_t name) -> hyperlink * {
  return global_atoms.create_hyperlink(name);
}

auto link(normal_atom *a, const size_t pos1, normal_atom *b, const size_t pos2)
    -> void {
  a->link_with(pos1, b, pos2);
  b->link_with(pos2, a, pos1);
}

void relink(const normal_atom *atom1, const size_t index1, normal_atom *atom2,
            const size_t index2) {
  if (const auto link = atom1->at(index1); link.is_normal()) {
    auto *na = link.get_normal();
    ::link(na, link.get_port(), atom2, index2);
  }
}

void unify(normal_atom const *atom1, size_t const index1,
           normal_atom const *atom2, size_t const index2) {
  if (const auto link = atom1->at(index1); link.is_normal()) {
    if (const auto link2 = atom2->at(index2); link2.is_normal()) {
      auto *na = link.get_normal();
      auto *na2 = link2.get_normal();
      ::link(na, link.get_port(), na2, link2.get_port());
    }
  }
}

auto find_atom(const name_t name, const int arity) {
  return global_atoms.find_atom(name, arity);
}

normal_atom *get_atom_at_port(const normal_atom *atom, const int port,
                              const name_t name, const int arity) {
  if (atom->at(port).get_normal()->get_arity() != arity or
      atom->at(port).get_normal()->get_label() != name) {
    return nullptr;
  }
  return atom->at(port).get_normal();
}

void remove_atom(normal_atom *atom) { global_atoms.remove_atom(atom); }

template <typename T> bool equals(T t1, T t2) { return t1 == t2; }

template <typename T> bool equals(T t1, T t2, T t3) {
  return t1 == t2 && t1 == t3;
}

template <typename T, typename... Args>
bool equals(T t1, T t2, T t3, Args... args) {
  return t1 == t2 && t1 == t3 && Compare(args...);
}

void dump_atoms() { std::cout << global_atoms.dump() << "\n"; }

