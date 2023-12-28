from __future__ import annotations
import random
from typing import (
    Any,
    Generator,
    List,
)


class AtomStore:
    store: dict[int, list[Atom]]

    def __init__(self):
        self.store = {}

    def create(self, name: str, arity: int) -> Atom:
        if arity not in self.store:
            self.store[arity] = list()
        for atom in self.store[arity]:
            # reuse removed atom
            if atom.name == name and atom.arity == arity and atom.removed:
                atom.removed = False
                return atom
        atom = Atom(name, arity)
        self.store[arity].append(atom)
        return atom

    def remove(self, atom: Atom):
        atom.removed = True

    def find(self, name: int, arity: int) -> Generator[Atom, None, None]:
        if arity not in self.store:
            return
        for atom in self.store[arity]:
            if atom.removed:
                continue
            if atom.name == name and atom.arity == arity:
                yield atom

    def print(self):
        for arity, atoms in self.store.items():
            for atom in atoms:
                if atom.removed:
                    continue
                print(atom)


class Atom:
    name: str
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
        return f"name: {self.name} arity: {self.arity} data: {self.data}"

    def at(self, index: int) -> Atom:
        return self.args[index]

    def remove_at(self, index: int):
        self.args[index].removed = True
        self.args[index] = None

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


atom_list: AtomStore = AtomStore()


def create_atom(name: int, arity: int) -> Atom:
    return atom_list.create(name, arity)


def remove_atom(atom: Atom):
    atom_list.remove(atom)


def find_atom(name: int, arity: int) -> Generator[Atom, None, None]:
    return atom_list.find(name, arity)


def link(atom: Atom, index: int, atom2: Atom, index2: int):
    atom.args[index] = atom2
    atom2.args[index2] = atom

def relink(atom: Atom, index: int, atom2: Atom, index2: int):
    atom3 = atom.args[index]
    index3 = 0
    for i in range(atom3.arity):
        if atom3.args[i] == atom:
            index3 = i
            break
    link(atom3, index3, atom2, index2)

def print_atoms():
    atom_list.print()

