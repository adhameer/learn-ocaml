(* Copy of the Parsetree structure, augmented with scoping and typing
   information for the constructs we use in class.

   Constructs NOT accepted by this library:
   - match-expressions with exception patterns
   - type extensions
   - recursive module declarations
   - class declarations, class types

   Constructs accepted by this library, but not recursed into nor
   augmented with extra information:
   - Types, type declarations (including module types)
   - Module expressions
   - Object declarations
   - Extensions

   Open-declarations are recursed into, but the module names
   are not augmented with scope/type information currently.
 *)

open Asttypes

type expression = {
  sexp_desc: expression_desc;
  sexp_env: Env.t;
  sexp_type: Types.type_expr;
  sexp_loc: Location.t;
  sexp_attrs: Parsetree.attributes;
}

and expression_desc =
  | Sexp_ident of Path.t * Longident.t loc
  | Sexp_constant of Parsetree.constant
  | Sexp_let of rec_flag * value_binding list * expression
  | Sexp_function of case list
  | Sexp_fun of arg_label * expression option * pattern * expression
  | Sexp_apply of expression * (arg_label * expression) list
  | Sexp_match of expression * case list
  | Sexp_try of expression * case list
  | Sexp_tuple of expression list
  | Sexp_construct of Longident.t loc * expression option
  | Sexp_variant of label * expression option
  | Sexp_record of (Longident.t loc * expression) list * expression option
  | Sexp_field of expression * Longident.t loc
  | Sexp_setfield of expression * Longident.t loc * expression
  | Sexp_array of expression list
  | Sexp_ifthenelse of expression * expression * expression option
  | Sexp_sequence of expression * expression
  | Sexp_while of expression * expression
  | Sexp_for of pattern * expression * expression * direction_flag * expression
  | Sexp_constraint of expression * Parsetree.core_type
  | Sexp_coerce of expression * Parsetree.core_type option * Parsetree.core_type
  | Sexp_send of expression * string loc
  | Sexp_new of Path.t * Longident.t loc * Types.class_declaration
  | Sexp_instvar of Path.t * Path.t * Longident.t loc
  | Sexp_setinstvar of Path.t * Path.t * string loc * expression
  | Sexp_override of Path.t * (Path.t * string loc * expression) list
  | Sexp_letmodule of Ident.t * string loc * module_expr * expression
  | Sexp_letexception of Parsetree.extension_constructor * expression
  | Sexp_assert of expression
  | Sexp_lazy of expression
  | Sexp_poly of expression * Parsetree.core_type option
  | Sexp_object of Parsetree.class_structure
  | Sexp_newtype of string loc * expression
  | Sexp_pack of module_expr
  | Sexp_open of override_flag * Longident.t loc * expression
  | Sexp_extension of Parsetree.extension
  | Sexp_unreachable

and value_binding = {
  svb_pat: pattern;
  svb_expr: expression
}

and case = {
  sc_lhs: pattern;
  sc_guard: expression option;
  sc_rhs: expression
}

and pattern =
  | Spat_any
  | Spat_var of Ident.t * string loc
  | Spat_alias of pattern * Ident.t * string loc
  | Spat_constant of Parsetree.constant
  | Spat_interval of Parsetree.constant * Parsetree.constant
  | Spat_tuple of pattern list
  | Spat_construct of Longident.t loc * pattern option
  | Spat_variant of label * pattern option
  | Spat_record of (Longident.t loc * pattern) list * closed_flag
  | Spat_array of pattern list
  | Spat_or of pattern * pattern
  | Spat_constraint of pattern * Parsetree.core_type
  | Spat_type of Longident.t loc
  | Spat_lazy of pattern
  | Spat_unpack of string loc
  | Spat_open of Longident.t loc * pattern

and module_expr =
  | Smod_ident of Longident.t loc
  | Smod_structure of structure
  | Smod_functor of string loc * Parsetree.module_type option * module_expr
  | Smod_apply of module_expr * module_expr
  | Smod_constraint of module_expr * Parsetree.module_type

and structure = {
  sstr_items: structure_item list;
  sstr_type: Types.signature;
  sstr_env: Env.t
}

and structure_item =
  | Sstr_eval of expression
  | Sstr_value of rec_flag * value_binding list
  | Sstr_primitive of Parsetree.value_description
  | Sstr_type of rec_flag * Parsetree.type_declaration list
  | Sstr_exception of Parsetree.extension_constructor
  | Sstr_module of module_binding
  | Sstr_modtype of Parsetree.module_type_declaration
  | Sstr_open of Parsetree.open_description
  | Sstr_include of module_expr

and module_binding = {
  smb_name: string loc;
  smb_expr: module_expr
}
