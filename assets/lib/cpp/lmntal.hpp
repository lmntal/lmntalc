#include <bitset>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <list>
#include <map>
#include <queue>
#include <random>
#include <ranges>
#include <sstream>
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

  [[nodiscard]] bool is_normal() const { return port >= 0; }
  [[nodiscard]] bool is_hyperlink() const { return port < 0; }
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
  auto get_int() const -> int64_t;
  [[nodiscard]] auto is_float() const -> bool;
  auto get_float() const -> double;
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
  explicit normal_atom(int32_t const label) : label(label) {}
  normal_atom(int32_t const label, size_t const bond_count)
      : label(label), bonds(bond_count) {}

  [[nodiscard]] auto at(size_t const pos) const -> link { return bonds[pos]; }
  auto link_with(size_t const pos, normal_atom *atom, size_t const atom_pos)
      -> void {
    bonds[pos].atom = atom;
    bonds[pos].port = static_cast<int64_t>(atom_pos);
  }
  auto link_with(size_t const pos, hyperlink *hlink) -> void {
    bonds[pos].atom = hlink;
    bonds[pos].port = -1;
  }
  auto remove_at(size_t const pos) -> void {
    auto *atom = bonds[pos].get_normal();
    atom->set_remove();
    bonds[pos].atom = nullptr;
  }
  [[nodiscard]] auto get_arity() const -> size_t { return bonds.size(); }
  [[nodiscard]] auto is_plain() const -> bool {
    return data.index() == plain_index;
  }
  [[nodiscard]] auto is_int() const -> bool {
    return data.index() == int_index;
  }
  [[nodiscard]] auto get_int() const -> int64_t {
    return std::get<int64_t>(data);
  }
  void set_int(int64_t value) { data = value; }
  [[nodiscard]] auto is_float() const -> bool {
    return data.index() == float_index;
  }
  [[nodiscard]] auto get_float() const -> double {
    return std::get<double>(data);
  }
  void set_float(double value) { data = value; }
  void set_remove() { flag = static_cast<atom_flag>(flag | atom_flag::Delete); }
  void restore_remove() {
    flag = static_cast<atom_flag>(flag & ~atom_flag::Delete);
  }
  [[nodiscard]] bool is_removed() const { return flag & atom_flag::Delete; }
  [[nodiscard]] hyperlink *get_hlink_at_port(size_t pos) const;

  [[nodiscard]] std::string to_string() const {
    switch (data.index()) {
    case int_index:
      return ::std::to_string(get_int());
    case float_index:
      return ::std::to_string(get_float());
    default:
      return std::string{int2str(label)};
    }
  }

  [[nodiscard]] std::vector<link> const &get_bonds() const { return bonds; }
  [[nodiscard]] name_t get_label() const { return label; }

private:
  friend hyperlink;
  friend atom_store;

  using data_type = std::variant<std::monostate, int64_t, double>;
  constexpr static size_t plain_index = 0;
  constexpr static size_t int_index = 1;
  constexpr static size_t float_index = 2;

  name_t label{};
  atom_flag flag{};
  data_type data{std::monostate{}};
  std::vector<link> bonds{};

  void clone_to(normal_atom *atom) const {
    atom->flag = flag;
    atom->bonds = bonds;
    atom->data = data;
  }
};

auto link::is_int() const -> bool {
  return is_normal() and get_normal()->is_int();
}
auto link::get_int() const -> int64_t { return get_normal()->get_int(); }
auto link::is_float() const -> bool {
  return is_normal() and get_normal()->is_float();
}
auto link::get_float() const -> double { return get_normal()->get_float(); }

auto hyperlink::add(normal_atom *atom, size_t const pos) -> void {
  atoms.insert(atom);
  atom->bonds[pos].atom = this;
  atom->bonds[pos].port = -1;
}

auto hyperlink::remove(normal_atom *atom, size_t const pos) -> void {
  atoms.erase(atom);
  atom->bonds[pos].atom = nullptr;
}

auto get_hlink_at_port(const normal_atom *atom, const size_t pos)
    -> hyperlink * {
  return atom->at(pos).get_hyperlink();
}

class atom_store {
public:
  atom_store() = default;

  auto create_atom(name_t const name, size_t const bond_count)
      -> normal_atom * {
    if (remove_queue_.contains(bond_count) and
        !remove_queue_[bond_count].empty()) {
      auto *a = remove_queue_[bond_count].front();
      remove_queue_[bond_count].pop();
      a->label = name;
      a->restore_remove();
      return a;
    }
    auto *a = new normal_atom{name, bond_count};

    if (not atoms_by_arity_.contains(bond_count)) {
      atoms_by_arity_[bond_count] = std::vector<normal_atom *>{};
    }

    atoms_by_arity_[bond_count].push_back(a);
    return a;
  }

  auto clone_atom(normal_atom const *atom, size_t const pos) -> normal_atom * {
    const auto nr = atom->at(pos).get_normal();
    auto *a = create_atom(nr->label, nr->get_arity());
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
    if (not remove_queue_.contains(atom->bonds.size())) {
      remove_queue_[atom->bonds.size()] = std::queue<normal_atom *>{};
    }
    remove_queue_[atom->bonds.size()].push(atom);
  }

  auto find_atom(name_t const name, int const arity)
      -> std::vector<normal_atom *> {
    auto res = std::vector<normal_atom *>{};
    for (auto *atom : atoms_by_arity_[arity]) {
      if (atom->label == name and not atom->is_removed()) {
        res.push_back(atom);
      }
    }
    return res;
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
  std::map<uint32_t, std::vector<normal_atom *>> atoms_by_arity_{};
  std::unordered_map<uint32_t, std::queue<normal_atom *>> remove_queue_{};
  std::unordered_set<hyperlink *> hyperlinks_{};

  static auto dfs_dump(normal_atom const *cur, std::stringstream &ss,
                       std::unordered_set<atom const *> &visited) -> void {
    visited.insert(cur);
    ss << cur->to_string() << "(";
    size_t count = 0;
    for (auto const &link : cur->get_bonds()) {
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

auto find_atom(const name_t name, const int arity)
    -> std::vector<normal_atom *> {
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

