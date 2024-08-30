    static Hyperlink getHyperlinkAtPort(Atom atom, int port) {
        var hl = atom.at(port).atom;
        if (hl instanceof Hyperlink) {
            return (Hyperlink) hl;
        }
        return null;
    }

    static Atom createAtom(int name, int arity) {
        return AtomStore.INSTANCE.createAtom(name, arity);
    }

    static NormalAtom cloneAtom(NormalAtom atom, int port) {
        return AtomStore.INSTANCE.cloneAtom(atom, port);
    }

    static void link(NormalAtom atom1, int index1, NormalAtom atom2, int index2) {
        atom1.set(index1, atom2, index2);
        atom2.set(index2, atom1, index1);
    }

    static void relink(NormalAtom atom1, int index1, NormalAtom atom2, int index2) {
        var link = atom1.at(index1);
        link((NormalAtom) link.atom, link.pos, atom2, index2);
    }

    static void unify(NormalAtom atom1, int index1, NormalAtom atom2, int index2) {
        var link1 = atom1.at(index1);
        var link2 = atom2.at(index2);
        link((NormalAtom) link1.atom, link1.pos, (NormalAtom) link2.atom, link2.pos);
    }

    static boolean equals(Atom... atoms) {
        for (int i = 0; i < atoms.length - 1; i++) {
            if (atoms[i] != atoms[i + 1]) {
                return false;
            }
        }
        return true;
    }

    static Stream<NormalAtom> findAtom(int name, int arity) {
        return AtomStore.INSTANCE.findAtom(name, arity);
    }

    static void dumpAtoms() {
        System.out.println(AtomStore.INSTANCE.dumpAtoms());
    }