# Humpless: No Bumps Along the Way

## Overview
Humpless is a simple and intuitive command-line interface (CLI) tool designed to help you manage your tasks and stay organized. Similar to Google Calendar, our app allows you to create, schedule, and prioritize tasks with ease. It helps keep track of done/in-progress/todo tasks and organize them by category, due date, and subtasks.

## Team Members
- Lawrence Granda Zarzuela ([lg626@cornell.edu](mailto:lg626@cornell.edu))
- Joyce Shen ([js3696@cornell.edu](mailto:js3696@cornell.edu))
- Lukas Friedman ([lsf58@cornell.edu](mailto:lsf58@cornell.edu))

## Project Manager
- James Kim ([jjk297@cornell.edu](mailto:jjk297@cornell.edu))

## Features
- **Task Management**: Create, edit, and delete tasks.
- **Task Attributes**: Each task includes a name, description, due date, category, and progress status.
- **Sorting and Filtering**: Sort and filter tasks based on different attributes like name, due date, and category.
- **Task Progress**: Mark tasks as done, in-progress, or to-do.
- **CSV Integration**: Load and save tasks from/to CSV files.

## Getting Started

### Prerequisites
- **OPAM**: Ensure you have OPAM installed. Follow the installation instructions on the [official OPAM website](https://opam.ocaml.org/doc/Install.html).
- **Dune**: Make sure you have Dune installed.

### Installation

1. **Update and Upgrade OPAM**:
   ```sh
   opam update
   opam upgrade
   ```

2. **Install Required Packages**:
   ```sh
   opam install ppx_inline_test csv ansiterminal
   ```

### Building and Running Humpless

1. **Build the Project**:
   ```sh
   dune build
   ```

2. **Execute Humpless**:
   ```sh
   dune exec bin/main.exe <csv in data>
   ```
   You should see a table printed in the terminal with the following columns:
   ```
   Name | Description | Date | Time | Category | Progress
   ```

### Running Tests

1. **Execute Tests**:
   ```sh
   dune test
   ```
   Ensure all tests pass.

## Test Plan

### Bisect Test Coverage
- **Overall**: 87.85%
- **Modules**:
  - **Table.ml**: Achieves 85% bisect coverage.
  - **Task.ml**: Achieves an impressive 98% bisect coverage.

### Automated Testing (OUnit)
- **Number of Tests**: 67 OUnit tests are conducted, ensuring a robust testing suite.
- **Approach**: Utilizes both BlackBox and GlassBox testing methodologies.
  - **Task.ml**: Tests setters, getters, creator, input validity, edge cases (e.g., invalid times or dates), and exception handling.
  - **Table.ml**: Tests creation, loaders, exception handling, sorting, setters, and filtering functionalities.

### Integration Testing
- **`Integration_test.ml`**: Located in the tests folder, automatically runs with `dune test`, performing integration testing on the Table module.
  - **Operations**: Executes multiple chained table operations to test the module's functionality.
  - **Validation**: The OUnit test passes if no exceptions are raised, providing a basic level of validation.
  - **Output Inspection**: Prints outputs, facilitating manual inspection to verify correctness.

### Demonstration of Correctness
- **Comprehensive Testing**: The test suite thoroughly covers the Table and Task modules, ensuring expected behavior.
- **Scenarios and Edge Cases**: Tests various scenarios and edge cases, including invalid inputs and exceptions.
- **Robustness**: Demonstrates the modules' correctness and robustness in managing tasks in a table format.
- **Combined Approach**: The combination of automated OUnit tests and manual integration testing provides a holistic validation, ensuring the modules function accurately and effectively.
