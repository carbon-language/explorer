// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface Has(T:! type) {
  fn Get() -> T;
}
impl i32 as Has(type) {
  fn Get() -> type { return Self; }
}

class WithType(T:! Has(type)) {
  fn Get() -> type { return T.Get(); }
}

fn Main() -> i32 {
  var v: WithType(i32).Get() = 0;
  return v;
}

// CHECK:STDOUT: result: 0
