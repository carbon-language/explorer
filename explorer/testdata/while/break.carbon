// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var x: i32 = 2;
  while (true) {
    if (x == 0) {
      break;
    } else {
      x = x - 1;
    }
  }
  return x;
}

// CHECK:STDOUT: result: 0
