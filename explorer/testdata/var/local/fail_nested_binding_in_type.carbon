// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  // CHECK:STDERR: COMPILATION ERROR: fail_nested_binding_in_type.carbon:[[@LINE+1]]: Binding types cannot contain bindings, but `T: type` is a binding.
  var x: (T: type) = 1;
  return 1;
}
