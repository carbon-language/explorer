// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface Vector {
  // CHECK:STDERR: COMPILATION ERROR: fail_member_lookup_in_definition.carbon:[[@LINE+1]]: incomplete type `interface Vector` used in member access
  fn ScalarZero() -> Vector.ScalarType;
  let ScalarType:! type;
}

fn Main() -> i32 {
  return 0;
}
