// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface X {
  fn F() -> type;
}

// CHECK:STDERR: COMPILATION ERROR: fail_use_symbolic_member.carbon:[[@LINE+1]]: member lookup for F in symbolic witness 0 of witness for T:! X
fn G[T:! X]() -> T.F() {
  return {};
}

fn Main() -> i32 {
  return 0;
}
