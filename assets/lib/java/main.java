    static Hyperlink getHyperlinkAtPort(Atom atom, int port) {
        var hl = atom.at(port);
        if (hl instanceof Hyperlink) {
            return (Hyperlink) hl;
        }
        return null;
    }

    static Atom createAtom(String name, int arity) {
        return AtomStore.INSTANCE.createAtom(name, arity);
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

    static boolean equals(Atom... atoms) {
        for (int i = 0; i < atoms.length - 1; i++) {
            if (atoms[i] != atoms[i + 1]) {
                return false;
            }
        }
        return true;
    }

    static Iterable<NormalAtom> findAtom(String name, int arity) {
        return AtomStore.INSTANCE.findAtom(name, arity);
    }

    static void dumpAtoms() {
        System.out.println(AtomStore.INSTANCE.dumpAtoms());
    }

    interface Rule {
        boolean apply();
    }