## Test Plan

### Bisect Test Coverage
- Overall: 87.85%
- Modules:
  - Table.ml: Achieves 85% bisect coverage.
  - Task.ml: Achieves an impressive 98% bisect coverage.

### Automated Testing (OUnit)
- **Number of Tests**: 67 OUnit tests are conducted, ensuring a robust testing suite.
- **Approach**: Utilizes both BlackBox and GlassBox testing methodologies.
  - Task.ml: Tests setters, getters, creator, input validity, edge cases (e.g., invalid times or dates), and exception handling.
  - Table.ml: Tests creation, loaders, exception handling, sorting, setters, and filtering functionalities.

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