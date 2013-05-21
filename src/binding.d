import std.conv;
import std.stdio;
import std.string;

import persistance;

class Environment {
public:
    this() {}
    this(string data) {
        Parser p = new Parser(this);
        foreach(string line; data.splitLines())
            p.parse(line);
        p.flush();
        this.scalars.rehash;
        this.vectors.rehash;
    }
    //TESTME
    //smoke test
    //empty input
    this(string[] filenames) {
        Parser p = new Parser(this);
        foreach(string filename; filenames) {
            auto file = File(filename);
            do {
                string line = file.readln();
                if (line.length == 0) break;
                p.parse(line);
            } while(true);
        }
        p.flush();
        this.scalars.rehash;
        this.vectors.rehash;
    }
    //TESTME

    void put_scalar(in string key, string value)
    in { assert(key !is null); }
    body {
        this.scalars[this.enforce_key(key)] = value;
    }
    //TESTME
    void put_vector(in string key, string[] value)
    in { assert(key !is null); }
    body {
        this.vectors[this.enforce_key(key)] = value;
    }
    //TESTME

    string get_scalar(in string key)
    in { assert(key !is null); }
    body {
        return scalars.get(key, null);
    }
    //TESTME
    string[] get_vector(in string key)
    in { assert(key !is null); }
    body {
        return vectors.get(key, null);
    }
    //TESTME


    string toString() {
        string acc;
        foreach(string key; this.scalars.byKey) {
            acc ~= "$" ~ key ~ "\n";
            foreach(string data; this.scalars[key].split("\n"))
                acc ~= ":" ~ data ~ "\n";
        }
        foreach(string key; this.vectors.byKey) {
            acc ~= "@" ~ key ~ "\n";
            bool first = true;
            foreach(string elem; this.vectors[key]) {
                string sigil = first ? ":" : ",";
                foreach(string data; elem.split("\n")) {
                    acc ~= sigil ~ data ~ "\n";
                    sigil = ":";
                }
                if (first) first = false;
            }
        }
        return acc;
    }
    //TESTME
    //TODO toFile, for more direct i/o

private:
    //SPIFFY and this why I like D
    string[string] scalars;
    string[][string] vectors;

    string enforce_key(in string key) {
        if (key.contains_any(".\n\r"))
            //TODO netter exception
            throw new Exception("Identifier cannot contain dot '.' or newline '\\n'/'\\r'");
        foreach(string k; this.scalars.byKey)
            if (k == key) throw new Exception("Duplicate identifier."); //TODO better exception
        foreach(string k; this.vectors.byKey)
            if (k == key) throw new Exception("Duplicate identifier."); //TODO better exception
        return key;
    }
    //TESTME

}


class SemanticEnvironment {
private:
    Environment outer;
public:
    this(Environment env) { this.outer = env; }

    string resolve_scalar(string[] path) {
        Environment env = this.outer;
        string acc;
        string sc; string[] vec;
        do {
            if ((sc = env.get_scalar(path[0])) !is null) {
                acc = sc;
                path = path[1..$];
            }
            else if ((vec = env.get_vector(path[0])) !is null) {
                if (path.length >= 2) {
                    { // validity/bounds checking
                        if (path[1].length == 0) throw new Exception("Not a whole number.");//TODO better diagnostics
                        foreach(char c; path[1])
                            if (!('0' <= c && c <= '9'))
                                throw new Exception("Not a whole number.");//TODO better diagnostics
                        if (path[1] == "0") throw new Exception("Not a whole number.");//TODO better diagnostics
                    }
                    size_t index = parse!size_t(path[1]);
                    if (index > vec.length) return "";
                    acc = vec[index - 1];
                    path = path[2..$];
                }
                else return to!string(vec.length);
            }
            else return "";
            if (path.length > 0)
                env = new Environment(acc);
            else break;
        } while(true);
        return acc;
    }//TESTME
    
    string[] resolve_vector(string[] path) {
        return []; //STUB
    }

}