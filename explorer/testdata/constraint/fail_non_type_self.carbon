// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

class X(T:! type) {}

// CHECK:STDERR: COMPILATION ERROR: fail_non_type_self.carbon:[[@LINE+1]]: `.Self` used in type of non-type generic binding `T`
fn F[T:! X(.Self)](x: T) {}

fn Main() -> i32 { return 0; }
