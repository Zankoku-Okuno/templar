
import dx.string : chomp, contains_any;
import std.string : strip;

import dx.testing;


final class SyntaxError : Exception {
    this(string msg, string file = __FILE__, ulong line = __LINE__, Throwable next = null) {
        super(msg, file, line, next);
    }
}

class Environment {
public:
    this() {}
    this(in string[] lines) {
        Parser p = new Parser(this);
        foreach(string line; lines)
            p.parse(line);
        p.flush();
    }
    //TESTME
    //smoke test
    //empty input

private:

    //SPIFFY and this why I like D
    string[string] scalars;
    string[][string] vectors;


    
}//Environment



private final
class Parser {
    this(Environment env) { this.outer = env; }
private:
    enum State { START, SCALAR, ARRAY, END }
    enum Sigil { DATA, SCALAR, ARRAY, NEXT }

    Environment outer;
    State state = State.START;
    bool got_first_line = false;
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
        //TODO disallow period in the value, dot syntax is used for recursive access
        return this._current_key = value;
    }
    unittest { 
        auto p = new Parser(new Environment());
        run_test({ p.current_key = "okay"; });
        run_test({ assert(does_throw!Exception(p.current_key = "")); });
    }

    void parse(in string raw)
    in { assert(raw !is null); }
    body {
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
        void reset() { p = new Parser(new Environment()); };
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

    void flush(State new_state=State.END)
    in { assert(this.state == State.START || this.current_key !is null); }
    out {
        assert(this.scalar_data == "");
        assert(this.array_data == []);
        assert(this.current_key is null);
        assert(!this.got_first_line);
    }
    body {
        if (this.state == State.START) return;
        else if (this.state == State.SCALAR) {
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
        this.got_first_line = false;
    }
    //no unittests, contracts are sufficient until integration tests

private:
    void buffer_data(in string data)
    in { assert(data !is null); }
    body {
        if (this.got_first_line) this.scalar_data ~= '\n';
        this.scalar_data ~= data;
        this.got_first_line = true;
    }
    //tested by parse function
    void buffer_elem() {
        this.array_data ~= this.scalar_data;
        this.scalar_data = "";
        this.got_first_line = false;
    }
    //tested by parse function

    static final
    struct Line {
    public:
        Sigil sigil;
        string data;
        this(in string raw)
        in { assert(raw !is null); }
        body {
            this.sigil = sigil_detect(raw);
            this.data = chomp(raw[1..$]);
        }
        unittest {
            run_test({
                auto l = Line(":$stuff");
                assert(l.sigil == Sigil.DATA);
                assert(l.data == "$stuff");
                l = Line(",:more stuff\r\n");
                assert(l.sigil == Sigil.NEXT);
                assert(l.data == ":more stuff");
            });
        }

    private:
        static
        Sigil sigil_detect(in string raw)
        in { assert(raw !is null); }
        body {
            if (raw.length < 1) throw new SyntaxError("Each line must begin with a sigil.");
            switch (raw[0]) {
                case ':': return Sigil.DATA;
                case '$': return Sigil.SCALAR;
                case '@': return Sigil.ARRAY;
                case ',': return Sigil.NEXT;
                default: throw new SyntaxError("Each line must begin with a sigil.");
            }
        }
        unittest {
            run_test({//Sigils detected correctly
                assert(sigil_detect(":a") == Sigil.DATA);
                assert(sigil_detect("$") == Sigil.SCALAR);
                assert(sigil_detect("@") == Sigil.ARRAY);
                assert(sigil_detect(",") == Sigil.NEXT);
            });
            run_test({//Non-sigils are syntax errors
                assert(does_throw!SyntaxError(sigil_detect("nope")));
                assert(does_throw!SyntaxError(sigil_detect("")));
            });
        }
    }//Line

}//Parser
