// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var p: {.x: i32, .y: i32} = {.x = 1, .y = 2};
  return p.y - p.x - 1;
}

// CHECK:STDOUT: result: 0
