// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var x: auto = 2;
  while (x != 0) {
    x = x - 1;
    continue;
    x = x + 1;
  }
  return x;
}

// CHECK:STDOUT: result: 0
