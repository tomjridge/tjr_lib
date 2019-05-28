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


(** {2 Logging and test control} *)

module Logger = Logger

module Log = Log

module Test = Test


(** {2 Misc} *)

module Map_int = Map.Make(struct type t = int let compare: t -> t -> int = Pervasives.compare end)

module Map_string = Map.Make(struct type t = string let compare: t -> t -> int = Pervasives.compare end)
