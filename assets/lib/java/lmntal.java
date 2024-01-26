import java.util.*;
import java.util.stream.Stream;

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
    protected int name;

    public int getName() {
        return name;
    }

    public void setName(int name) {
        this.name = name;
    }

    public abstract int getArity();

    public Link at(int index) {
        throw new UnsupportedOperationException();
    }

    public boolean isInt() {
        return false;
    }

    public long getInt() {
        throw new UnsupportedOperationException();
    }

    public void setInt(long value) {
        throw new UnsupportedOperationException();
    }

    public boolean isFloat() {
        return false;
    }

    public double getFloat() {
        throw new UnsupportedOperationException();
    }

    public void setFloat(double value) {
        throw new UnsupportedOperationException();
    }

    public void setChar(char value) {
        throw new UnsupportedOperationException();
    }

    public static final class Link {
        Atom atom;
        int pos;

        public boolean isInt() {
            return atom.isInt();
        }

        public long getInt() {
            return atom.getInt();
        }

        public boolean isFloat() {
            return atom.isFloat();
        }

        public double getFloat() {
            return atom.getFloat();
        }
    }
}

final class Hyperlink extends Atom {
    private final HashSet<NormalAtom> atoms;

    public Hyperlink(int name) {
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
        if (atoms.isEmpty()) {
            AtomStore.INSTANCE.removeHyperlink(this);
        }
    }

    @Override
    public int getArity() {
        return atoms.size();
    }
}

final class NormalAtom extends Atom {
    private final Link[] args;
    private Object data;
    private DataType type;
    private boolean isRemoved;

    public NormalAtom(int name, int arity) {
        this.name = name;
        this.type = DataType.PLAIN;
        this.args = new Link[arity];
        for (int i = 0; i < arity; i++) {
            args[i] = new Link();
        }
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

    public NormalAtom getAtomAtPort(int port, int name, int arity) {
        var atom = at(port);
        if (atom == null) {
            return null;
        }
        if (atom.atom.getName() == name && atom.atom.getArity() == arity) {
            return (NormalAtom) atom.atom;
        }
        return null;
    }

    public Link at(int index) {
        return args[index];
    }

    public void set(int index, Hyperlink atom) {
        args[index].atom = atom;
        args[index].pos = -1;
    }

    public void set(int index, NormalAtom atom, int pos) {
        args[index].atom = atom;
        args[index].pos = pos;
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
        AtomStore.INSTANCE.removeAtom((NormalAtom) args[index].atom);
        args[index].atom = null;
    }

    public void cloneFrom(NormalAtom atom) {
        this.data = atom.data;
        this.type = atom.type;
        System.arraycopy(atom.args, 0, this.args, 0, args.length);
    }

    @Override
    public String toString() {
        return switch (type) {
            case INT -> Long.toString(getInt());
            case FLOAT -> Double.toString(getFloat());
            default -> Main.getName(name);
        };
    }

    enum DataType {
        PLAIN,
        INT,
        FLOAT,
        CHAR,
        STRING,
    }
}

class AtomStore {
    public static AtomStore INSTANCE = new AtomStore();
    private final HashMap<Integer, List<NormalAtom>> atoms;
    private final HashMap<Integer, Queue<NormalAtom>> reusableAtoms;
    private final HashSet<Hyperlink> hyperlinks;
    private final Queue<Hyperlink> reusableHyperlinks;

    public AtomStore() {
        this.atoms = new HashMap<>();
        this.hyperlinks = new HashSet<>();
        this.reusableAtoms = new HashMap<>();
        this.reusableHyperlinks = new ArrayDeque<>();
    }

    public NormalAtom createAtom(int name, int arity) {
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

    public Hyperlink createHyperlink(int name) {
        if (!reusableHyperlinks.isEmpty()) {
            var hl = reusableHyperlinks.poll();
            hl.setName(name);
            return hl;
        }
        var hl = new Hyperlink(name);
        hyperlinks.add(hl);
        return hl;
    }

    public void removeHyperlink(Hyperlink hl) {
        reusableHyperlinks.add(hl);
    }

    public void removeAtom(NormalAtom atom) {
        if (!reusableAtoms.containsKey(atom.getArity())) {
            reusableAtoms.put(atom.getArity(), new ArrayDeque<>());
        }
        atom.remove();
        var queue = reusableAtoms.get(atom.getArity());
        queue.add(atom);
    }

    public Stream<NormalAtom> findAtom(int name, int arity) {
        if (!atoms.containsKey(arity)) {
            return Stream.empty();
        }
        return atoms.get(arity)
                .stream()
                .filter(atom -> atom.getName() == name && !atom.isRemoved());
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
            var atom = cur.at(i).atom;
            if (atom instanceof Hyperlink) {
                sb.append(atom.name);
                sb.append(",");
                count++;
            } else if (atom instanceof NormalAtom nr) {
                if (!visited.contains(nr)) {
                    dfsDump(nr, sb, visited);
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

    public NormalAtom cloneAtom(NormalAtom atom, int port) {
        var newAtom = createAtom(atom.at(port).atom.getName(), atom.at(port).atom.getArity());
        newAtom.cloneFrom((NormalAtom) atom.at(port).atom);
        return newAtom;
    }
}

