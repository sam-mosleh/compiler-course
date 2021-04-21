class List {
    isNil() : Bool { true };
    head()  : Int { { abort(); 0; } };
    tail()  : List { { abort(); self; } };
    cons(i : Int) : List {
        (new Cons).init(i, self)
    };

    remove_first(value: Int): List {
        if ( value = head() ) then {
            tail();
        } else {
            tail().remove_first(value).cons(head());
        }
        fi
    };

    min(): Int {
        if ( tail().isNil() ) then {
            head();
        } else {
            let
                rest_min: Int <- tail().min()
            in {
                if ( head() < rest_min ) then {
                    head();
                } else {
                    rest_min;
                }
                fi;
            };
        }
        fi
    };

    has(value: Int): Bool {
        if ( isNil() ) then {
            false;
        } else {
            if ( value = head() ) then {
                true;
            } else {
                tail().has(value);
            }
            fi;
        }
        fi
    };
};

class Cons inherits List {
    car : Int;
    cdr : List;
    isNil() : Bool { false };
    head()  : Int { car };
    tail()  : List { cdr };
    init(i : Int, rest : List) : List {
        {
            car <- i;
            cdr <- rest;
            self;
        }
   };
};



class Main {
    io: IO <- new IO;

    print_list(l : List): IO {
        if ( l.isNil() ) then {
            io.out_string("\n");
        } else {
                io.out_int(l.head());
                io.out_string(" ");
                print_list(l.tail());
        }
        fi
    };

    remove_repeateds_from(l: List): List {
        let
            unique_list: List <- new List
        in {
            while ( not l.isNil() )
            loop {
                if ( not unique_list.has(l.head()) ) then {
                    unique_list <- unique_list.cons(l.head());
                    l <- l.tail();
                } else {
                    l <- l.tail();
                }
                fi;
            }
            pool;
            unique_list;
        }
    };

    main(): IO {
        {
            let
                input_list : List <- new List,
                n: Int <- io.in_int(),
                i: Int <- 0
            in {
                while ( i < n )
                loop {
                    input_list <- input_list.cons(io.in_int());
                    i <- i + 1;
                }
                pool;
                print_list(remove_repeateds_from(input_list));
            };
        }
    };
};
