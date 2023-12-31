from __future__ import annotations
import random
from typing import (
    Any,
    Generator,
    List,
)


class Atom(object):
    name: str
    pass


class NormalAtom(Atom):
    arity: int
    data: Any
    args: List[Atom]
    removed: bool = False

    def __init__(self, name: str, arity: int):
        self.name = name
        self.arity = arity
        self.data = None
        self.args = [None] * arity
        self.removed = False

    def __str__(self):
        if isinstance(self.data, int) or isinstance(self.data, float):
            return f"{self.data}"
        return f"{self.name}"

    def at(self, index: int) -> Atom:
        return self.args[index]

    def remove_at(self, index: int):
        self.args[index].removed = True
        self.args[index] = None

    def is_plain(self) -> bool:
        return self.data is None

    def is_int(self) -> bool:
        return self.data is not None and isinstance(self.data, int)

    def set_int(self, data: int):
        self.data = data

    def get_int(self) -> int:
        return self.data

    def is_float(self) -> bool:
        return self.data is not None and isinstance(self.data, float)

    def is_str(self) -> bool:
        return self.data is not None and isinstance(self.data, str)


class Hyperlink(Atom):
    args: set[NormalAtom]

    def __init__(self, name: str):
        self.name = name
        self.args = set()

    def add(self, atom: NormalAtom, index: int):
        self.args.add(atom)
        atom.args[index] = self

    def remove(self, atom: NormalAtom, index: int):
        self.args.remove(atom)
        atom.args[index] = None


class AtomStore:
    store: dict[int, list[NormalAtom]]
    hyperlinks: list[Hyperlink]

    def __init__(self):
        self.store = {}
        self.hyperlinks = []

    def create(self, name: str, arity: int) -> NormalAtom:
        if arity not in self.store:
            self.store[arity] = list()
        for atom in self.store[arity]:
            # reuse removed atom
            if atom.name == name and atom.arity == arity and atom.removed:
                atom.removed = False
                return atom
        atom = NormalAtom(name, arity)
        self.store[arity].append(atom)
        return atom

    def create_hyperlink(self, name: str) -> Hyperlink:
        hl = Hyperlink(name)
        self.hyperlinks.append(hl)
        return hl

    def remove(self, atom: NormalAtom):
        atom.removed = True

    def find(self, name: int, arity: int) -> Generator[NormalAtom, None, None]:
        if arity not in self.store:
            return
        for atom in self.store[arity]:
            if atom.removed:
                continue
            if atom.name == name and atom.arity == arity:
                yield atom

    def dump(self) -> str:
        visited = set()
        res = ""
        for _, atoms in self.store.items():
            for atom in atoms:
                if not atom.is_plain or atom.removed or (atom in visited):
                    continue
                res += self.__dfs_dump(atom, visited, res)
        return res

    def __dfs_dump(self, atom: Atom, visited: set[NormalAtom], string: str) -> str:
        visited.add(atom)
        string += str(atom)
        string += "("
        count = 0
        for i in range(atom.arity):
            atom2 = atom.at(i)
            if isinstance(atom2, Hyperlink):
                string += atom2.name
                string += ","
                count += 1
            elif atom2 is not None and atom2 not in visited:
                string = self.__dfs_dump(atom2, visited, string)
                string += ","
                count += 1
        string = string[:-1]
        if count != 0:
            string += ")"
        return string


atom_list: AtomStore = AtomStore()


def create_atom(name: int, arity: int) -> NormalAtom:
    return atom_list.create(name, arity)


def create_hyperlink(name: str) -> Hyperlink:
    return atom_list.create_hyperlink(name)


def remove_atom(atom: NormalAtom):
    atom_list.remove(atom)


def find_atom(name: int, arity: int) -> Generator[NormalAtom, None, None]:
    return atom_list.find(name, arity)


def link(atom: NormalAtom, index: int, atom2: NormalAtom, index2: int):
    atom.args[index] = atom2
    atom2.args[index2] = atom


def relink(atom: NormalAtom, index: int, atom2: NormalAtom, index2: int):
    atom3 = atom.args[index]
    index3 = 0
    for i in range(atom3.arity):
        if atom3.args[i] == atom:
            index3 = i
            break
    link(atom3, index3, atom2, index2)


def get_hyperlink_at_port(atom: NormalAtom, index: int) -> Hyperlink:
    atom2 = atom.at(index)
    if isinstance(atom2, Hyperlink):
        return atom2
    return None


def equals(*atoms: Atom) -> bool:
    for i in range(len(atoms) - 1):
        if atoms[i] != atoms[i + 1]:
            return False
    return True


def dump_atoms():
    print(atom_list.dump())

