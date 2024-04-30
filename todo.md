# TODO

cloc --by-file  --exclude-dir=_build --include-lang=OCaml .

LAWRENCE 4.28 Sun-Wed 5.01
- table operations:
  - sorting by: name, due-date, anything
  - filtering
  - add indexing (show an id in the table for each task, no need to be stored in csv, but it could) to support deletion of tasks
  - delete tasks
  - check a task (use built-in task function, to be coded)
  - modify specific task fields
- encapsulate table
- comment

JOYCE 5.01 Wed-Sun
- main program:
  - rewrite using new table module, not table_ops
  - infinite command-line operations, each time ask user for input
    - save input after each manipulation into a file
    - exit on ctrl-d
  - generate key words/dictionary of commands (`delete <idx>` or `sort <column> [ascending/descending]`, etc.)
    - accepted commands, one per table operation
    - print table after each command
- comment code

LUKAS 5.01 Wed-Sun
- testing:
  - adapt testing to the new table and task modules
  - dependent on encapsulation

- create a Makefile

- if we need more lines of code:
  - adding columns
  - table look pretty

