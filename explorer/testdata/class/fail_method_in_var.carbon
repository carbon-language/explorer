// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

class C {
  fn F() {}
  fn G[self: Self]() {}
}

fn Main() -> i32 {
  var f: auto = C.F;
  // CHECK:STDERR: COMPILATION ERROR: fail_method_in_var.carbon:[[@LINE+1]]: member name G can only be used in a member access or alias
  var g: auto = C.G;

  return 0;
}
