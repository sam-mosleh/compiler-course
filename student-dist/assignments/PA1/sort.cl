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

    sorted(l: List): List {
        let
            sorted_list: List <- new List
        in {
            while ( not l.isNil() )
            loop {
                let minimum: Int <- l.min() in {
                    sorted_list <- sorted_list.cons(minimum);
                    l <- l.remove_first(minimum);
                };
            }
            pool;
            sorted_list;
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
                print_list(sorted(input_list));
            };
        }
    };
};
