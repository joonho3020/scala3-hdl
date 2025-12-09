package hdl

import utest.*

object ModuleChecksSpec extends TestSuite:
  val tests = Tests {
    test("simple_module_test") { simple_module_test() }
    test("list_operation_check") { list_operation_check() }
    test("nested_module_check") { nested_module_check() }
    test("nested_bundle_check") { nested_bundle_check() }
    test("inheritance_check") { inheritance_check() }
    test("type_parameterization_check") { type_parameterization_check() }
    test("conditional_generation_check") { conditional_generation_check() }
    test("optional_io_check") { optional_io_check() }
    test("nested_seq_generation_check") { nested_seq_generation_check() }
    test("optional_and_map_check") { optional_and_map_check() }
    test("module_array_generation_check") { module_array_generation_check() }
    test("parameter_sweep_check") { parameter_sweep_check() }
    test("when_behavior_check") { when_behavior_check() }
    test("comparison_operator_check") { comparison_operator_check() }
    test("vec_check") { vec_check() }
  }
