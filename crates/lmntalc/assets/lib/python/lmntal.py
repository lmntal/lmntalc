from __future__ import annotations
import random
import copy
from typing import (
    Any,
    Generator,
    List,
)


class Atom(object):
    name: int
    pass


class Link:
    atom: Atom
    port: int

    def __init__(self, atom: Atom, port: int):
        self.atom = atom
        self.port = port

    def is_int(self):
        if isinstance(self.atom, NormalAtom):
            return self.atom.is_int()
        return False

    def get_int(self) -> int:
        if isinstance(self.atom, NormalAtom):
            return self.atom.get_int()
        return 0


class NormalAtom(Atom):
    arity: int
    data: Any
    args: List[Link]
    removed: bool = False

    def __init__(self, name: int, arity: int):
        self.name = name
        self.arity = arity
        self.data = None
        self.args = list()
        for i in range(arity):
            self.args.append(Link(None, 0))
        self.removed = False

    def __str__(self):
        if isinstance(self.data, int) or isinstance(self.data, float):
            return f"{self.data}"
        return name_map[self.name]

    def arity(self) -> int:
        return self.arity

    def at(self, index: int) -> Link:
        return self.args[index]

    def remove_at(self, index: int):
        self.args[index].atom.removed = True
        self.args[index].atom = None

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

    def __init__(self, name: int):
        self.name = name
        self.args = set()

    def add(self, atom: NormalAtom, index: int):
        self.args.add(atom)
        atom.args[index].atom = self

    def remove(self, atom: NormalAtom, index: int):
        self.args.remove(atom)
        atom.args[index].atom = None

    def arity(self) -> int:
        return len(self.args)


class AtomStore:
    store: dict[int, list[NormalAtom]]
    hyperlinks: list[Hyperlink]

    def __init__(self):
        self.store = {}
        self.hyperlinks = []

    def create(self, name: int, arity: int) -> NormalAtom:
        if arity not in self.store:
            self.store[arity] = list()
        for atom in self.store[arity]:
            # reuse removed atom
            if atom.removed:
                atom.removed = False
                atom.name = name
                return atom
        atom = NormalAtom(name, arity)
        self.store[arity].append(atom)
        return atom

    def clone(self, atom: NormalAtom, port: int) -> NormalAtom:
        target = atom.args[port].atom # type: NormalAtom
        atom2 = self.create(target.name, target.arity)
        atom2.data = copy.deepcopy(target.data)
        for i in range(atom2.arity):
            atom2.args[i] = copy.deepcopy(target.args[i])
        return atom2

    def create_hyperlink(self, name: int) -> Hyperlink:
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

    def __dfs_dump(self, atom: NormalAtom, visited: set[NormalAtom], string: str) -> str:
        visited.add(atom)
        string += str(atom)
        string += "("
        count = 0
        for i in range(atom.arity):
            atom2 = atom.at(i).atom
            if isinstance(atom2, Hyperlink):
                string += name_map[atom2.name]
                string += ","
                count += 1
            elif isinstance(atom2, NormalAtom) and (atom2 not in visited):
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


def clone_atom(atom: NormalAtom, port: int) -> NormalAtom:
    return atom_list.clone(atom, port)


def create_hyperlink(name: int) -> Hyperlink:
    return atom_list.create_hyperlink(name)


def remove_atom(atom: NormalAtom):
    atom_list.remove(atom)


def find_atom(name: int, arity: int) -> Generator[NormalAtom, None, None]:
    return atom_list.find(name, arity)


def link(atom: NormalAtom, index: int, atom2: NormalAtom, index2: int):
    atom.args[index].atom = atom2
    atom.args[index].port = index2
    atom2.args[index2].atom = atom
    atom2.args[index2].port = index


def relink(atom: NormalAtom, index: int, atom2: NormalAtom, index2: int):
    atom3 = atom.args[index]
    link(atom3.atom, atom3.port, atom2, index2)


def unify(atom: NormalAtom, index: int, atom2: NormalAtom, index2: int):
    atom3 = atom.args[index]
    atom4 = atom2.args[index2]
    link(atom3.atom, atom3.port, atom4.atom, atom4.port)


def get_atom_at_port(atom: NormalAtom, index: int, name: str, arity: int) -> NormalAtom | None:
    atom2 = atom.at(index)
    if isinstance(atom2, NormalAtom) and atom2.name == name and atom2.arity == arity:
        return atom2
    return None


def get_hyperlink_at_port(atom: NormalAtom, index: int) -> Hyperlink | None:
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

