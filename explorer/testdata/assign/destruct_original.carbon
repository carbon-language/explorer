// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

// Test that assignment performs a copy and does not create an alias.

fn Main() -> i32 {
  var x: i32 = -1;
  if (true) {
    var y: i32 = 0;
    x = y;
    // y dies here
  }
  return x;
}

// CHECK:STDOUT: result: 0
