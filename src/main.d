import std.uni : lineSep, paraSep;



final class SyntaxError : Exception {
    this(string msg, string file = __FILE__, ulong line = __LINE__, Throwable next = null) {
        super(msg, file, line, next);
    }
}

class Environment {
    this(in string[] lines) {
        Parser p = new Parser();
        foreach(string line; lines)
            p.parse(Line(line));
        p.flush();
    }
    //TESTME

    private:
    enum State { START, SCALAR, ARRAY, END }
    enum Sigil { DATA, SCALAR, ARRAY, NEXT }
    
    //SPIFFY and this why I like D
    string[string] scalars;
    string[][string] vectors;

    class Parser {
        State state = State.START;
        string current_key = null; //TODO restrict key to be length >=1, not contain newlines
        string scalar_data = "";
        string[] array_data = [];

        void parse(in Line line) {
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
                        case Sigil.DATA: buffer_data(line.data); break;
                        case Sigil.NEXT: this.buffer_elem(); break;
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
        //TESTME

        void buffer_data(string data) {
            if (this.scalar_data != "") this.scalar_data ~= '\n';
            this.scalar_data ~= data;
        }
        void buffer_elem() {
            this.array_data ~= this.scalar_data;
            this.scalar_data = "";
        }

        void flush(State new_state=State.END)
        in {
            assert(this.state != State.START);
            assert(current_key !is null);
        }
        out {
            assert(this.scalar_data == "");
            assert(this.array_data == []);
        }
        body {
            if (this.state == State.SCALAR) {
                this.outer.scalars[current_key] = this.scalar_data;
                this.scalar_data = "";
            }
            else {
                assert(this.state == State.ARRAY);
                this.buffer_elem();
                this.outer.vectors[current_key] = this.array_data;
                this.array_data = [];
            }
            this.state = new_state;
        }
        //TESTME
    }//Parser
    
    static struct Line {
    public:
        Sigil sigil;
        string data;
        this(in string raw) {
            this.sigil = sigil_detect(raw);
            this.data = line_chomp(raw);
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
        static
        string line_chomp(in string raw) {
            //FIXME why can't I compile it using just a straight raw.chomp()?
            size_t i = raw.length;
            switch (raw[$-1]) {
                    case '\n':
                        if (raw.length >= 2 && raw[$-2] == '\r') {
                                i -= 2;
                                break;
                            }
                    case '\r': case lineSep: case paraSep:
                        i -= 1;
                        break;
                    default: break;
                }
            return raw[1..i];
        }
        //TESTME
    }//Line
}//Environment




