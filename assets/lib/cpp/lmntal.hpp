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
#include <vector>

using rule = bool (*)();

enum atom_flag : uint8_t {
  Delete = 1 << 0,
};

enum atom_type : uint8_t { Plain, Int, Float, String, Char };

class Atom {
public:
  explicit Atom(std::string_view const label) : label{label} {}
  virtual ~Atom() = default;
  virtual void set_remove() = 0;
  [[nodiscard]] virtual size_t get_arity() const = 0;
  void set_label(std::string_view const label) { this->label = label; }
  [[nodiscard]] auto get_label() const -> std::string_view { return label; }
  virtual void set_at(size_t pos, Atom *atom) {
    throw std::runtime_error("Not supported");
  }
  virtual bool is_int() const { return false; }
  virtual int64_t get_int() const { throw std::runtime_error("Not supported"); }
  virtual bool is_float() const { return false; }
  virtual double get_float() const { throw std::runtime_error("Not supported"); }
  std::string label;
};

class Hyperlink;
class NormalAtom;
class AtomStore;

class NormalAtom final : public Atom {
public:
  explicit NormalAtom(std::string_view const label) : Atom(label) {}
  NormalAtom(std::string_view const label, size_t const bond_count)
      : Atom(label), bonds(bond_count) {}

  [[nodiscard]] auto at(size_t const pos) const -> Atom * { return bonds[pos]; }
  auto set_at(size_t const pos, Atom *atom) -> void override {
    bonds[pos] = atom;
  }
  auto remove_at(size_t const pos) -> void {
    bonds[pos]->set_remove();
    bonds[pos] = nullptr;
  }
  [[nodiscard]] auto get_arity() const -> size_t override {
    return bonds.size();
  }
  [[nodiscard]] auto is_plain() const -> bool {
    return type == atom_type::Plain;
  }
  [[nodiscard]] auto is_int() const -> bool override { return type == atom_type::Int; }
  [[nodiscard]] auto get_int() const -> int64_t override {
    return *static_cast<int64_t *>(data);
  }
  void set_int(int64_t value) {
    data = new int64_t{value};
    type = atom_type::Int;
  }
  [[nodiscard]] auto is_float() const -> bool override {
    return type == atom_type::Float;
  }
  [[nodiscard]] auto get_float() const -> double override {
    return *static_cast<double *>(data);
  }
  void set_float(double value) {
    data = new double{value};
    type = atom_type::Float;
  }
  [[nodiscard]] auto get_string() const -> std::string {
    return *static_cast<std::string *>(data);
  }
  void set_string(std::string value) const {
    *static_cast<std::string *>(data) = std::move(value);
  }
  [[nodiscard]] auto get_char() const -> char {
    return *static_cast<char *>(data);
  }
  void set_char(char const value) const { *static_cast<char *>(data) = value; }
  void set_remove() override {
    flag = static_cast<atom_flag>(flag | atom_flag::Delete);
  }
  void restore_remove() {
    flag = static_cast<atom_flag>(flag & ~atom_flag::Delete);
  }
  [[nodiscard]] bool is_removed() const { return flag & atom_flag::Delete; }
  [[nodiscard]] Hyperlink *get_hlink_at_port(size_t pos) const;

  [[nodiscard]] std::string to_string() const {
    switch (type) {
    case Int:
      return ::std::to_string(get_int());
    case Float:
      return ::std::to_string(get_float());
    default:
      return label;
    }
  }

  [[nodiscard]] std::vector<Atom *> get_bonds() const { return bonds; }

private:
  friend Hyperlink;
  friend AtomStore;
  atom_flag flag{};
  atom_type type{atom_type::Plain};
  void *data{};
  std::vector<Atom *> bonds{};
};

class Hyperlink final : public Atom {
public:
  explicit Hyperlink(std::string_view const label) : Atom(label) {}

  auto add(NormalAtom *atom, size_t const pos) -> void {
    atoms.insert(atom);
    atom->bonds[pos] = this;
  }

  auto remove(NormalAtom *atom, size_t const pos) -> void {
    atoms.erase(atom);
    atom->bonds[pos] = nullptr;
  }

  void set_remove() override {}
  [[nodiscard]] auto get_arity() const -> size_t override {
    return atoms.size();
  }

private:
  std::unordered_set<NormalAtom *> atoms;
};

auto get_hlink_at_port(const NormalAtom *atom, const size_t pos)
    -> Hyperlink * {
  return dynamic_cast<Hyperlink *>(atom->at(pos));
}

class AtomStore {
public:
  AtomStore() = default;

  auto create_atom(std::string_view const name, size_t const bond_count)
      -> NormalAtom * {
    if (remove_queue_.contains(bond_count) and
        !remove_queue_[bond_count].empty()) {
      auto *a = remove_queue_[bond_count].front();
      remove_queue_[bond_count].pop();
      a->label = std::string{name};
      a->restore_remove();
      return a;
    }
    auto *a = new NormalAtom{name, bond_count};

    if (not atoms_by_arity_.contains(bond_count)) {
      atoms_by_arity_[bond_count] = std::vector<NormalAtom *>{};
    }

    atoms_by_arity_[bond_count].push_back(a);
    return a;
  }

  auto create_hyperlink(std::string_view const name) -> Hyperlink * {
    auto *hl = new Hyperlink{name};
    hyperlinks_.insert(hl);
    return hl;
  }

  void remove_atom(NormalAtom *atom) {
    atom->set_remove();
    if (not remove_queue_.contains(atom->bonds.size())) {
      remove_queue_[atom->bonds.size()] = std::queue<NormalAtom *>{};
    }
    remove_queue_[atom->bonds.size()].push(atom);
  }

  auto find_atom(std::string_view const name, int const arity)
      -> std::vector<NormalAtom *> {
    auto res = std::vector<NormalAtom *>{};
    for (auto *atom : atoms_by_arity_[arity]) {
      if (atom->label == name and not atom->is_removed()) {
        res.push_back(atom);
      }
    }
    return res;
  }

  [[nodiscard]] auto dump() const -> std::string {
    std::stringstream res;
    std::unordered_set<Atom const *> visited;
    for (const auto &atoms : atoms_by_arity_ | std::views::values) {
      for (auto const *atom : atoms) {
        if (not atom->is_plain() or atom->is_removed() or
            visited.contains(atom)) {
          continue;
        }
        dfs_dump(atom, res, visited);
      }
    }
    return res.str();
  }

private:
  std::map<uint32_t, std::vector<NormalAtom *>> atoms_by_arity_{};
  std::unordered_map<uint32_t, std::queue<NormalAtom *>> remove_queue_{};
  std::unordered_set<Hyperlink *> hyperlinks_{};

  static auto dfs_dump(NormalAtom const *cur, std::stringstream &ss,
                       std::unordered_set<Atom const *> &visited) -> void {
    visited.insert(cur);
    ss << cur->to_string() << "(";
    size_t count = 0;
    for (auto const *b : cur->get_bonds()) {
      if (b != nullptr and !visited.contains(b)) {
        // check if the atom is a hyperlink
        if (auto *na = dynamic_cast<NormalAtom const *>(b)) {
          dfs_dump(na, ss, visited);
          ss << ",";
          count++;
        } else if (auto *hl = dynamic_cast<Hyperlink const *>(b)) {
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

AtomStore global_atoms{};

auto create_atom(const std::string_view name, const size_t bond_count)
    -> NormalAtom * {
  return global_atoms.create_atom(name, bond_count);
}

auto create_hyperlink(const std::string_view name) -> Hyperlink * {
  return global_atoms.create_hyperlink(name);
}

auto link(Atom *a, const size_t pos1, Atom *b, const size_t pos2) -> void {
  a->set_at(pos1, b);
  b->set_at(pos2, a);
}

void relink(const NormalAtom *atom1, const size_t index1, NormalAtom *atom2,
            const size_t index2) {
  const auto atom3 = atom1->at(index1);
  // find the port of the atom1 which is linked to the atom3
  if (auto *na = dynamic_cast<NormalAtom *>(atom3)) {
    auto index3 = 0;
    for (int i = 0; i < na->get_arity(); i++) {
      if (dynamic_cast<NormalAtom *>(na->at(i)) == atom1) {
        index3 = i;
        break;
      }
    }
    link(na, index3, atom2, index2);
  }
}

auto find_atom(const std::string_view name, const int arity)
    -> std::vector<NormalAtom *> {
  return global_atoms.find_atom(name, arity);
}

NormalAtom *get_atom_at_port(const NormalAtom *atom, const int port,
                             const std::string_view name, const int arity) {
  if (atom->at(port)->get_arity() != arity or atom->at(port)->label != name) {
    return nullptr;
  }
  return dynamic_cast<NormalAtom *>(atom->at(port));
}

void remove_atom(NormalAtom *atom) { global_atoms.remove_atom(atom); }

template <typename T> bool equals(T t1, T t2) { return t1 == t2; }

template <typename T> bool equals(T t1, T t2, T t3) {
  return t1 == t2 && t1 == t3;
}

template <typename T, typename... Args>
bool equals(T t1, T t2, T t3, Args... args) {
  return t1 == t2 && t1 == t3 && Compare(args...);
}

void dump_atoms() { std::cout << global_atoms.dump() << "\n"; }

