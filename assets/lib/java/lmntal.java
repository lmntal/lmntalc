import java.util.*;
import java.util.stream.Collectors;

interface Rule {
    boolean apply();
}

enum DataType {
    PLAIN,
    INT,
    FLOAT,
    CHAR,
    STRING,
}

class Atom {
    private final Atom[] args;
    private String name;
    private Object data;
    private DataType type;
    private boolean isRemoved;

    public Atom(String name, int arity) {
        this.name = name;
        this.type = DataType.PLAIN;
        this.args = new Atom[arity];
    }

    public static boolean equals(Atom... atoms) {
        for (int i = 0; i < atoms.length - 1; i++) {
            if (atoms[i] != atoms[i + 1]) {
                return false;
            }
        }
        return true;
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

    public long getInt() {
        return (long) data;
    }

    public double getFloat() {
        return (double) data;
    }

    public char getChar() {
        return (char) data;
    }

    public String getString() {
        return (String) data;
    }

    public boolean isPlain() {
        return type == DataType.PLAIN;
    }

    public boolean isInt() {
        return type == DataType.INT;
    }

    public void setInt(long value) {
        this.data = value;
        this.type = DataType.INT;
    }

    public boolean isFloat() {
        return type == DataType.FLOAT;
    }

    public void setFloat(double value) {
        this.data = value;
        this.type = DataType.FLOAT;
    }

    public boolean isChar() {
        return type == DataType.CHAR;
    }

    public void setChar(char value) {
        this.data = value;
        this.type = DataType.CHAR;
    }

    public boolean isString() {
        return type == DataType.STRING;
    }

    public void setString(String value) {
        this.data = value;
        this.type = DataType.STRING;
    }

    public int getArity() {
        return args.length;
    }

    public void removeAt(int index) {
        AtomStore.INSTANCE.removeAtom(args[index]);
        args[index] = null;
    }

    @Override
    public String toString() {
        return switch (type) {
            case INT -> Long.toString(getInt());
            case FLOAT -> Double.toString(getFloat());
            default -> name;
        };
    }
}

class AtomStore {
    public static AtomStore INSTANCE = new AtomStore();
    private final HashMap<Integer, List<Atom>> atoms;
    private final HashMap<Integer, Queue<Atom>> reusableAtoms;

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

    public String dumpAtoms() {
        var sb = new StringBuilder();
        var visited = new HashSet<Atom>();
        for (var atoms_by_arity : atoms.values()) {
            for (var atom : atoms_by_arity) {
                if (!atom.isPlain() || atom.isRemoved() || visited.contains(atom)) {
                    continue;
                }
                dfsDump(atom, sb, visited);
            }
        }
        return sb.toString();
    }

    private void dfsDump(Atom cur, StringBuilder sb, HashSet<Atom> visited) {
        visited.add(cur);
        sb.append(cur.toString());
        sb.append("(");
        var count = 0;
        for (int i = 0; i < cur.getArity(); i++) {
            var atom = cur.at(i);
            if (atom != null && !visited.contains(atom)) {
                dfsDump(atom, sb, visited);
                sb.append(",");
                count++;
            }
        }
        sb.deleteCharAt(sb.length() - 1);
        if (count != 0) {
            sb.append(")");
        }
    }
}

