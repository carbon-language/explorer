// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var t: auto = (5, 2);
  match (t) {
    case (a: auto, b: auto) => {
      return a + b - 7;
    }
  }
  return 1;
}

// CHECK:STDOUT: result: 0
