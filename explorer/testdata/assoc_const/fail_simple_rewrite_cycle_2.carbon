// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface TwoTypes {
  let T:! type;
  let U:! type;
}

// Attempting to fully resolve the replacement for `.T` would never create a
// situation where `.T`'s expansion involves `.T`. Ensure we catch this anyway.
// CHECK:STDERR: COMPILATION ERROR: fail_simple_rewrite_cycle_2.carbon:[[@LINE+1]]: rewrite of (X).(TwoTypes.U) applies within its own resolved expansion of (X).(TwoTypes.U)*
fn F[X:! TwoTypes where .U = .U* and .T = .U]() {}

fn Main() -> i32 {
  return 0;
}
