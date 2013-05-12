
import dx.string : chomp, contains_any;
import std.string : strip;

import dx.testing;


final class SyntaxError : Exception {
    this(string msg, string file = __FILE__, ulong line = __LINE__, Throwable next = null) {
        super(msg, file, line, next);
    }
}
    enum State { START, SCALAR, ARRAY, END }
    enum Sigil { DATA, SCALAR, ARRAY, NEXT }

class Environment {
    this(in string[] lines) {
        Parser p = new Parser(this);
        foreach(string line; lines)
            p.parse(line);
        p.flush();
    }
    //TESTME

    private:

    //SPIFFY and this why I like D
    string[string] scalars;
    string[][string] vectors;


    
}//Environment



private final
class Parser {
    this(Environment env=null) { this.outer = env; }
private:
    Environment outer;
    State state = State.START;
    private string _current_key = null;
    string scalar_data = "";
    string[] array_data = [];
    
public:
    @property string current_key() { return this._current_key; }
    @property string current_key(string value)
    in {
        assert(value !is null);
        assert(!value.contains_any("\n\r"));
    }
    body {
        value = value.strip();
        if (value.length == 0) throw new Exception("Zero-length identifiers not allowed.");
        return this._current_key = value;
    }
    unittest { 
        auto p = new Parser();
        run_test({ p.current_key = "okay"; });
        run_test({ assert(does_throw!Exception(p.current_key = "")); });
    }

    void parse(in string raw) {
        auto line = Line(raw);
        switch (this.state) {
            case State.START: {
                switch (line.sigil) {
                    case Sigil.DATA: throw new SyntaxError("Data encountered before key established.");
                    case Sigil.NEXT: throw new SyntaxError("Array delimiter encountered before array established.");
                    case Sigil.SCALAR:
                        this.current_key = line.data;
                        this.state = State.SCALAR;
                        break;
                    case Sigil.ARRAY:
                        this.current_key = line.data;
                        this.state = State.ARRAY;
                        break;
                    default: assert(false);
                }
                break;
            }
            case State.SCALAR: {
                switch (line.sigil) {
                    case Sigil.DATA: buffer_data(line.data); break;
                    case Sigil.NEXT: throw new SyntaxError("Array delimiter encountered in scalar.");
                    case Sigil.SCALAR:
                        this.flush(State.SCALAR);
                        this.current_key = line.data;
                        break;
                    case Sigil.ARRAY:
                        this.flush(State.ARRAY);
                        this.current_key = line.data;
                        break;
                    default: assert(false);
                }
                break;
            }
            case State.ARRAY: {
                switch (line.sigil) {
                    case Sigil.NEXT: this.buffer_elem(); //fallthrough
                    case Sigil.DATA: this.buffer_data(line.data); break;
                    case Sigil.SCALAR:
                        this.flush(State.SCALAR);
                        this.current_key = line.data;
                        break;
                    case Sigil.ARRAY:
                        this.flush(State.ARRAY);
                        this.current_key = line.data;
                        break;
                    default: assert(false);
                }
                break;
            }
            case State.END:
                //TODO better exception type
                throw new Exception("Further parsing attempted after end of input reached.");
            default: assert(false);
        }
    }//Parser.parse
    unittest {
        Parser p;
        void reset() { p = new Parser(); };
        run_test({//Scalar state accumulates data
            reset();
            p.parse("$scalar\n");
            assert(p.state == State.SCALAR);
            assert(p.current_key == "scalar");
            p.parse(":hello\n");
            p.parse(":goodbye\n");
            assert(p.current_key == "scalar");
            assert(p.scalar_data == "hello\ngoodbye");
        });
        run_test({//Scalar state transitions correctly
            reset();
            p.parse("$scalar\n");
            p.parse("$new scalar\n");
            assert(p.state == State.SCALAR);
            assert(p.current_key == "new scalar");
            p.parse("@new array\n");
            assert(p.state == State.ARRAY);
            assert(p.current_key == "new array");
        });
        run_test({//Array state accumulates data
            reset();
            p.parse("@array\n");
            assert(p.state == State.ARRAY);
            assert(p.current_key == "array");
            p.parse(":part1\n");
            p.parse(":part2");
            assert(p.current_key == "array");
            assert(p.scalar_data == "part1\npart2");
        });
        run_test({//Array state accumulates elements
            reset();
            p.parse("@array\n");
            p.parse(":part1\n");
            p.parse(":part2\n");
            p.parse(",elem2\n");
            p.parse(",");
            assert(p.current_key == "array");
            assert(p.array_data == ["part1\npart2", "elem2"]);
        });
        run_test({//Array state transitions correctly
            reset();
            p.parse("@array\n");
            p.parse("@new array\n");
            assert(p.state == State.ARRAY);
            assert(p.current_key == "new array");
            p.parse("$new scalar\n");
            assert(p.state == State.SCALAR);
            assert(p.current_key == "new scalar");
        });
        run_test({//All states reject invalid input.
            reset();
            assert(does_throw!SyntaxError(p.parse("gobbledygook")));
            assert(does_throw!SyntaxError(p.parse(":data\n")));
            assert(does_throw!SyntaxError(p.parse(",elem\n")));
            p.parse("$scalar\n");
            assert(does_throw!SyntaxError(p.parse("gobbledygook")));
            assert(does_throw!SyntaxError(p.parse(",elem")));
            p.parse("@array\n");
            assert(does_throw!SyntaxError(p.parse("gobbledygook")));
        });
        run_test({//Blank lines are not a problem
            reset();
            p.parse("$scalar\n");
            p.parse(":\n");
            p.parse(":goodbye\n");
            p.parse(":");
            assert(p.scalar_data == "\ngoodbye\n");
        });
        run_test({//Empty data is not a problem
            reset();
            p.parse("$scalar\n");
            p.parse(":");
            assert(p.scalar_data == "");
        });
    }
    //TESTME

    void flush(State new_state=State.END)
    in {
        assert(this.state != State.START);
        assert(this.current_key !is null);
    }
    out {
        assert(this.scalar_data == "");
        assert(this.array_data == []);
        assert(this.current_key is null);
    }
    body {
        if (this.state == State.SCALAR) {
            if (this.outer !is null)
                this.outer.scalars[current_key] = this.scalar_data;
            this.scalar_data = "";
        }
        else {
            assert(this.state == State.ARRAY);
            this.buffer_elem();
            if (this.outer !is null)
                this.outer.vectors[current_key] = this.array_data;
            this.array_data = [];
        }
        this._current_key = null;
        this.state = new_state;
    }
    //no unittests, contracts are sufficient until integration tests

private:
    void buffer_data(string data) {
        if (this.scalar_data != "") this.scalar_data ~= '\n';
        this.scalar_data ~= data;
    }
    //TESTME
    void buffer_elem() {
        this.array_data ~= this.scalar_data;
        this.scalar_data = "";
    }
    //TESTME

    static final
    struct Line {
    public:
        Sigil sigil;
        string data;
        this(in string raw) {
            this.sigil = sigil_detect(raw);
            this.data = chomp(raw[1..$]);
        }
        //TESTME

    private:
        static
        Sigil sigil_detect(in string raw) {
            if (raw.length < 1) throw new SyntaxError("Each line must begin with a sigil.");
            switch (raw[0]) {
                case ':': return Sigil.DATA;
                case '$': return Sigil.SCALAR;
                case '@': return Sigil.ARRAY;
                case ',': return Sigil.NEXT;
                default: throw new SyntaxError("Each line must begin with a sigil.");
            }
        }
        //TESTME
    }//Line

    }//Parser
