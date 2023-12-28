    static void removeAtom(Atom atom) {
        AtomStore.INSTANCE.removeAtom(atom);
    }

    static void link(Atom atom1, int index1, Atom atom2, int index2) {
        atom1.set(index1, atom2);
        atom2.set(index2, atom1);
    }

    static void relink(Atom atom1, int index1, Atom atom2, int index2) {
        var atom3 = atom1.at(index1);
        // find the port of the atom1 which is linked to the atom3
        var index3 = 0;
        for (int i = 0; i < atom3.getArity(); i++) {
            if (atom3.at(i) == atom1) {
                index3 = i;
                break;
            }
        }
        link(atom3, index3, atom2, index2);
    }

    static Iterable<Atom> findAtom(String name, int arity) {
        return AtomStore.INSTANCE.findAtom(name, arity);
    }

    static void printAtoms() {
        AtomStore.INSTANCE.printAtoms();
    }