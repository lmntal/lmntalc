import java.util.*;
import java.util.stream.Collectors;

interface Rule {
    boolean apply();
}

enum DataType {
    INT,
    FLOAT,
    CHAR,
    STRING,
}

class Atom {
    private String name;
    private final Atom[] args;
    private Object data;
    private DataType type;
    private boolean isRemoved;

    public Atom(String name, int arity) {
        this.name = name;
        this.args = new Atom[arity];
    }

    public boolean isRemoved() {
        return isRemoved;
    }

    public void remove() {
        isRemoved = true;
    }

    public void restore() {
        isRemoved = false;
    }

    public Atom getAtomAtPort(int port, String name, int arity) {
        var atom = at(port);
        if (atom == null) {
            return null;
        }
        if (atom.getName().equals(name) && atom.getArity() == arity) {
            return atom;
        }
        return null;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Atom at(int index) {
        return args[index];
    }

    public void set(int index, Atom atom) {
        args[index] = atom;
    }

    public int getInt() {
        return (int) data;
    }

    public float getFloat() {
        return (float) data;
    }

    public char getChar() {
        return (char) data;
    }

    public String getString() {
        return (String) data;
    }

    public boolean isInt() {
        return type == DataType.INT;
    }

    public boolean isFloat() {
        return type == DataType.FLOAT;
    }

    public boolean isChar() {
        return type == DataType.CHAR;
    }

    public boolean isString() {
        return type == DataType.STRING;
    }

    public void setInt(int value) {
        this.data = value;
        this.type = DataType.INT;
    }

    public void setFloat(float value) {
        this.data = value;
        this.type = DataType.FLOAT;
    }

    public void setChar(char value) {
        this.data = value;
        this.type = DataType.CHAR;
    }

    public void setString(String value) {
        this.data = value;
        this.type = DataType.STRING;
    }

    public int getArity() {
        return args.length;
    }

    public static boolean equals(Atom... atoms) {
        for (int i = 0; i < atoms.length - 1; i++) {
            if (atoms[i] != atoms[i + 1]) {
                return false;
            }
        }
        return true;
    }

    public void removeAt(int index) {
        AtomStore.INSTANCE.removeAtom(args[index]);
        args[index] = null;
    }
}

class AtomStore {
    private final HashMap<Integer, List<Atom>> atoms;
    private final HashMap<Integer, Queue<Atom>> reusableAtoms;

    public static AtomStore INSTANCE = new AtomStore();

    public AtomStore() {
        this.atoms = new HashMap<>();
        this.reusableAtoms = new HashMap<>();
    }

    public Atom createAtom(String name, int arity) {
        if (reusableAtoms.containsKey(arity)) {
            var queue = reusableAtoms.get(arity);
            if (!queue.isEmpty()) {
                var atom = queue.poll();
                atom.setName(name);
                atom.restore();
                return atom;
            }
        }
        var atom = new Atom(name, arity);
        if (!atoms.containsKey(arity)) {
            atoms.put(arity, new ArrayList<>());
        }
        atoms.get(arity).add(atom);
        return atom;
    }

    public void removeAtom(Atom atom) {
        if (!reusableAtoms.containsKey(atom.getArity())) {
            reusableAtoms.put(atom.getArity(), new LinkedList<>());
        }
        atom.remove();
        var queue = reusableAtoms.get(atom.getArity());
        queue.add(atom);
    }

    public Iterable<Atom> findAtom(String name, int arity) {
        if (!atoms.containsKey(arity)) {
            return new ArrayList<>();
        }
        return atoms.get(arity)
                .stream()
                .filter(atom -> atom.getName().equals(name) && !atom.isRemoved())
                .collect(Collectors.toList());
    }

    public void printAtoms() {
        for (var arity : atoms.keySet()) {
            for (var atom : atoms.get(arity)) {
                if (atom.isRemoved()) {
                    continue;
                }
                System.out.print(atom.getName());
                System.out.print("(");
                for (int i = 0; i < atom.getArity(); i++) {
                    if (atom.at(i) == null) {
                        System.out.print("null");
                    } else {
                        System.out.print(atom.at(i).getName());
                    }
                    if (i != atom.getArity() - 1) {
                        System.out.print(", ");
                    }
                }
                System.out.println(")");
            }
        }
    }
}

