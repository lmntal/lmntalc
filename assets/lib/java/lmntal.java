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

abstract class Atom {
    protected String name;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public abstract int getArity();

    public void set(int index, Atom atom) {
        throw new UnsupportedOperationException();
    }

    public Atom at(int index) {
        throw new UnsupportedOperationException();
    }

    public boolean isInt() {
        return false;
    }

    public void setInt(long value) {
        throw new UnsupportedOperationException();
    }

    public long getInt() {
        throw new UnsupportedOperationException();
    }

    public void setFloat(double value) {
        throw new UnsupportedOperationException();
    }

    public void setChar(char value) {
        throw new UnsupportedOperationException();
    }
}

final class Hyperlink extends Atom {
    private final HashSet<NormalAtom> atoms;

    public Hyperlink(String name) {
        this.atoms = new HashSet<>();
        this.name = name;
    }

    public void add(NormalAtom atom, int pos) {
        atoms.add(atom);
        atom.set(pos, this);
    }

    public void remove(NormalAtom atom, int pos) {
        atoms.remove(atom);
        atom.set(pos, null);
    }

    @Override
    public int getArity() {
        return 1;
    }
}

final class NormalAtom extends Atom {
    private final Atom[] args;
    private Object data;
    private DataType type;
    private boolean isRemoved;

    public NormalAtom(String name, int arity) {
        this.name = name;
        this.type = DataType.PLAIN;
        this.args = new Atom[arity];
    }

    public static boolean equals(NormalAtom... atoms) {
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

    public NormalAtom getAtomAtPort(int port, String name, int arity) {
        var atom = at(port);
        if (atom == null) {
            return null;
        }
        if (atom.getName().equals(name) && atom.getArity() == arity) {
            return (NormalAtom) atom;
        }
        return null;
    }


    public Atom at(int index) {
        return args[index];
    }

    public void set(int index, Atom atom) {
        args[index] = atom;
    }

    public void set(int index, NormalAtom atom) {
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
        AtomStore.INSTANCE.removeAtom((NormalAtom) args[index]);
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
    private final HashMap<Integer, List<NormalAtom>> atoms;
    private final HashMap<Integer, Queue<NormalAtom>> reusableAtoms;
    private final HashSet<Hyperlink> hyperlinks;

    public AtomStore() {
        this.atoms = new HashMap<>();
        this.hyperlinks = new HashSet<>();
        this.reusableAtoms = new HashMap<>();
    }

    public NormalAtom createAtom(String name, int arity) {
        if (reusableAtoms.containsKey(arity)) {
            var queue = reusableAtoms.get(arity);
            if (!queue.isEmpty()) {
                var atom = queue.poll();
                atom.setName(name);
                atom.restore();
                return atom;
            }
        }
        var atom = new NormalAtom(name, arity);
        if (!atoms.containsKey(arity)) {
            atoms.put(arity, new ArrayList<>());
        }
        atoms.get(arity).add(atom);
        return atom;
    }

    public Hyperlink createHyperlink(String name) {
        var hl = new Hyperlink(name);
        hyperlinks.add(hl);
        return hl;
    }

    public void removeAtom(NormalAtom atom) {
        if (!reusableAtoms.containsKey(atom.getArity())) {
            reusableAtoms.put(atom.getArity(), new LinkedList<>());
        }
        atom.remove();
        var queue = reusableAtoms.get(atom.getArity());
        queue.add(atom);
    }

    public Iterable<NormalAtom> findAtom(String name, int arity) {
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
        var visited = new HashSet<NormalAtom>();
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

    private void dfsDump(NormalAtom cur, StringBuilder sb, HashSet<NormalAtom> visited) {
        visited.add(cur);
        sb.append(cur.toString());
        sb.append("(");
        var count = 0;
        for (int i = 0; i < cur.getArity(); i++) {
            var atom = cur.at(i);
            if (atom instanceof Hyperlink) {
                sb.append(atom.name);
                sb.append(",");
                count++;
            } else if (atom instanceof NormalAtom) {
                if (!visited.contains(atom)) {
                    dfsDump((NormalAtom) atom, sb, visited);
                    sb.append(",");
                    count++;
                }
            }
        }
        sb.deleteCharAt(sb.length() - 1);
        if (count != 0) {
            sb.append(")");
        }
    }
}

