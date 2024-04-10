# OPAM Packages:

1. Update and upgrade OPAM with `$ opam update` and `$ opam upgrade`.
2. Install the required packages with `$ opam install ppx_inline_test csv ansiterminal`.


# Running Humpless

1. Make sure you have dune installed.
2. Run `$ dune build` in the root directory of the projects using the terminal.
3. RUn `$ dune exec bin/main.exe` in your terminal.

You should see a table being printed in the terminal. Make sure you see the followings columns:

Name | Description | Date | Time | Category | Progress

# Running Tests

1. Run `$ dune test` (all of them should pass)
