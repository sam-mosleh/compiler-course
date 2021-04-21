class Main {
    io: IO <- new IO;

    main(): IO {
        {
            let
                par_string: String <- io.in_string(),
                index: Int <- 0,
                par_counter: Int <- 0,
                is_valid: Bool <- true
            in {
                while ( index < par_string.length() )
                loop
                {
                    if ( par_string.substr(index, 1) = "(" ) then {
                        par_counter <- par_counter + 1;
                    } else {
                        if ( 0 < par_counter ) then {
                            par_counter <- par_counter - 1;
                        } else {
                            is_valid <- false;
                        }
                        fi;
                    }
                    fi;
                    index <- index + 1;
                }
                pool;

                if ( 0 < par_counter ) then {
                    is_valid <- false;
                } else {
                    0;
                }
                fi;

                if ( is_valid ) then {
                    io.out_string("Yes\n");
                } else {
                    io.out_string("No\n");
                }
                fi;
            };
        }
    };
};
