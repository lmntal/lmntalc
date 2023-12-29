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
#include <vector>

using rule = bool (*)();

enum atom_flag : uint16_t {
  Delete = 1 << 0,
  Hyperlink = 1 << 1,
};

enum atom_type : uint16_t { Plain, Int, Float, String, Char };

struct atom {
  std::string label;
  atom_flag flag{};
  atom_type type{atom_type::Plain};
  void *data{};
  std::vector<atom *> bonds{};

  atom(std::string_view label) : label{label} {}
  atom(std::string_view label, size_t bond_count)
      : label{label}, bonds(bond_count) {}

  auto at(size_t pos) const -> atom const *const { return bonds[pos]; }
  auto at(size_t pos) -> atom * { return bonds[pos]; }
  auto remove_at(size_t pos) -> void {
    bonds[pos]->set_remove();
    bonds[pos] = nullptr;
  }
  auto get_arity() const -> size_t { return bonds.size(); }
  auto is_plain() const -> bool { return type == atom_type::Plain; }
  auto is_int() const -> bool { return type == atom_type::Int; }
  auto get_int() const -> int64_t { return *static_cast<int64_t *>(data); }
  void set_int(int64_t value) {
    data = new int64_t{value};
    type = atom_type::Int;
  }
  auto is_float() const -> bool { return type == atom_type::Float; }
  auto get_float() const -> double { return *static_cast<double *>(data); }
  void set_float(double value) {
    data = new double{value};
    type = atom_type::Float;
  }
  auto get_string() const -> std::string {
    return *static_cast<std::string *>(data);
  }
  void set_string(std::string value) {
    *static_cast<std::string *>(data) = value;
  }
  auto get_char() const -> char { return *static_cast<char *>(data); }
  void set_char(char value) { *static_cast<char *>(data) = value; }
  void set_remove() { flag = static_cast<atom_flag>(flag | atom_flag::Delete); }
  void restore_remove() {
    flag = static_cast<atom_flag>(flag & ~atom_flag::Delete);
  }
  bool is_removed() const { return flag & atom_flag::Delete; }

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

  [[nodiscard]] std::vector<atom *> get_bonds() const { return bonds; }
};

class atom_store {
public:
  atom_store() = default;

  auto create_atom(std::string_view const name, size_t const bond_count)
      -> atom * {
    if (remove_queue_.contains(bond_count) and
        !remove_queue_[bond_count].empty()) {
      auto *a = remove_queue_[bond_count].front();
      remove_queue_[bond_count].pop();
      a->label = std::string{name};
      a->restore_remove();
      return a;
    }
    auto *a = new atom{name, bond_count};

    if (not atoms_by_arity_.contains(bond_count)) {
      atoms_by_arity_[bond_count] = std::vector<atom *>{};
    }

    atoms_by_arity_[bond_count].push_back(a);
    return a;
  }

  void remove_atom(atom *atom) {
    atom->set_remove();
    if (not remove_queue_.contains(atom->bonds.size())) {
      remove_queue_[atom->bonds.size()] = std::queue<class atom *>{};
    }
    remove_queue_[atom->bonds.size()].push(atom);
  }

  auto find_atom(std::string_view name, int arity) -> std::vector<atom *> {
    auto res = std::vector<atom *>{};
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
  std::map<uint32_t, std::vector<atom *>> atoms_by_arity_{};
  std::unordered_map<uint32_t, std::queue<atom *>> remove_queue_{};

  static auto dfs_dump(atom const *cur, std::stringstream &ss,
                       std::unordered_set<atom const *> &visited) -> void {
    visited.insert(cur);
    ss << cur->to_string() << "(";
    size_t count = 0;
    for (auto const *b : cur->get_bonds()) {
      if (b != nullptr and !visited.contains(b)) {
        dfs_dump(b, ss, visited);
        ss << ",";
        count++;
      }
    }
    ss.seekp(-1, std::ios_base::end);
    if (count != 0) {
      ss << ")";
    }
  }
};

atom_store global_atoms{};

auto create_atom(std::string_view name, size_t bond_count) -> atom * {
  return global_atoms.create_atom(name, bond_count);
}

auto link(atom *a, size_t pos1, atom *b, size_t pos2) -> void {
  a->bonds[pos1] = b;
  b->bonds[pos2] = a;
}

void relink(atom *atom1, size_t index1, atom *atom2, size_t index2) {
  auto atom3 = atom1->at(index1);
  // find the port of the atom1 which is linked to the atom3
  auto index3 = 0;
  for (int i = 0; i < atom3->get_arity(); i++) {
    if (atom3->at(i) == atom1) {
      index3 = i;
      break;
    }
  }
  link(atom3, index3, atom2, index2);
}

auto find_atom(std::string_view name, int arity) -> std::vector<atom *> {
  return global_atoms.find_atom(name, arity);
}

atom *get_atom_at_port(atom *atom, int port, std::string_view name, int arity) {
  if (atom->bonds[port]->bonds.size() != arity or
      atom->bonds[port]->label != name) {
    return nullptr;
  }
  return atom->bonds[port];
}

void remove_atom(atom *atom) { global_atoms.remove_atom(atom); }

template <typename T> bool equals(T t1, T t2) { return t1 == t2; }

template <typename T> bool equals(T t1, T t2, T t3) {
  return t1 == t2 && t1 == t3;
}

template <typename T, typename... Args>
bool equals(T t1, T t2, T t3, Args... args) {
  return t1 == t2 && t1 == t3 && Compare(args...);
}

void dump_atoms() { std::cout << global_atoms.dump() << "\n"; }

