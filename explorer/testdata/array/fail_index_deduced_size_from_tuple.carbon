// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var x: [i32;] = (0, 1);
  // CHECK:STDERR: RUNTIME ERROR: fail_index_deduced_size_from_tuple.carbon:[[@LINE+1]]: index 2 out of range in (0, 1)
  return x[2];
}
