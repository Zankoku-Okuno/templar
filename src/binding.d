import std.stdio;
import std.string;

import persistance;

class Environment {
public:
    this() {}
    this(in string[] lines) {
        Parser p = new Parser(this);
        foreach(string line; lines)
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
        return scalars.get(key, "");
    }
    //TESTME
    string[] get_vector(in string key)
    in { assert(key !is null); }
    body {
        return vectors.get(key, []);
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

    //STUB
}