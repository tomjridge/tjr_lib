(** Interface to [tjr_lib_core] package functionality. *)

(** {2 Prelude} *)

include Util

module Int_ = Int_

module List_ = List_

module String_ = String_

(** {2 Extras} *)

module Tjr_map = Tjr_map

module Exhaustive_testing = Exhaustive_testing

module Gensym = Gensym

module Global = Global

module Tjr_store = Tjr_store
type fstore = Tjr_store.fstore (* available after open Tjr_lib_core *)

(* module Init_ref = Init_ref *)

(** {2 Logging and test control} *)

module Logger = Logger

module Log = Log

module Test = Test

