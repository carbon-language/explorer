// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface Iface { let N:! i32; }

fn PickType(N: i32) -> type { return i32; }

fn F[T:! Iface](x: T) -> i32 {
  // CHECK:STDERR: COMPILATION ERROR: fail_unknown_value.carbon:[[@LINE+1]]: value of associated constant (T).(Iface.N) is not known
  var x: PickType(T.N) = 0;
  return x;
}

impl i32 as Iface where .N = 5 {}

fn Main() -> i32 {
  return F(0);
}
