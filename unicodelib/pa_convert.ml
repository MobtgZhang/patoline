open Earley_core
open Earley
open Pa_ocaml_prelude
open Ast_helper
module Ext(In:Extension) =
  struct
    include In
    let blank str pos =
      let rec fn state ((str, pos) as cur) =
        let (c, str', pos') = Input.read str pos in
        let next = (str', pos') in
        match (state, c) with
        | (_, '\255') -> cur
        | (`Ini, (' '|'\t'|'\r')) -> fn `Ini next
        | (`Ini, '#') -> fn `Com next
        | (`Ini, _) -> cur
        | (`Com, '\n') -> fn `Ini next
        | (`Com, _) -> fn `Com next in
      fn `Ini (str, pos)
    let ex_int =
      Earley_core.Earley.fsequence
        (Earley_str.regexp ~name:"0x[0-9a-fA-F]+" "0x[0-9a-fA-F]+"
           (fun group -> group 0))
        (Earley_core.Earley.empty (fun i -> int_of_string i))
    let mapping =
      change_layout
        (Earley_core.Earley.fsequence ex_int
           (Earley_core.Earley.fsequence_ignore
              (Earley_str.regexp ~name:"[ \\t]*" "[ \t]*"
                 (fun group -> group 0))
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.option (-1) ex_int)
                 (Earley_core.Earley.empty (fun j -> fun i -> (i, j))))))
        no_blank
    let build_file _loc ms =
      let loc = _loc in
      let combine (i, j) e =
        Exp.sequence ~loc
          (Exp.apply ~loc
             (Exp.ident ~loc { loc; txt = (Ldot ((Lident "Array"), "set")) })
             [(Nolabel, (Exp.ident ~loc { loc; txt = (Lident "arr") }));
             (Nolabel, (Exp.constant ~loc (Const.int i)));
             (Nolabel, (Exp.constant ~loc (Const.int j)))]) e in
      let arr_fill =
        List.fold_right combine ms
          (Exp.ident ~loc { loc; txt = (Lident "arr") }) in
      let conversion_array_expr =
        Exp.let_ ~loc Nonrecursive
          [Vb.mk (Pat.var ~loc { loc; txt = "arr" })
             (Exp.apply ~loc
                (Exp.ident ~loc
                   { loc; txt = (Ldot ((Lident "Array"), "make")) })
                [(Nolabel, (Exp.constant ~loc (Const.int 256)));
                (Nolabel, (Exp.constant ~loc (Const.int (-1))))])] arr_fill in
      let raise_undef =
        Exp.apply ~loc (Exp.ident ~loc { loc; txt = (Lident "raise") })
          [(Nolabel,
             (Exp.construct ~loc { loc; txt = (Lident "Undefined") } None))] in
      let ty_int = Typ.constr ~loc { loc; txt = (Lident "int") } [] in
      let ty_int_array =
        Typ.constr ~loc { loc; txt = (Lident "array") } [ty_int] in
      let ty_char = Typ.constr ~loc { loc; txt = (Lident "char") } [] in
      let ty_uchar =
        Typ.constr ~loc { loc; txt = (Ldot ((Lident "UChar"), "uchar")) } [] in
      let e_i = Exp.ident ~loc { loc; txt = (Lident "i") } in
      let e_c = Exp.ident ~loc { loc; txt = (Lident "c") } in
      let e_s = Exp.ident ~loc { loc; txt = (Lident "s") } in
      let e_u = Exp.ident ~loc { loc; txt = (Lident "u") } in
      let e_arr = Exp.ident ~loc { loc; txt = (Lident "conversion_array") } in
      let cmp_lt a b =
        Exp.apply ~loc (Exp.ident ~loc { loc; txt = (Lident "<") })
          [(Nolabel, a); (Nolabel, b)] in
      let cmp_gt a b =
        Exp.apply ~loc (Exp.ident ~loc { loc; txt = (Lident ">") })
          [(Nolabel, a); (Nolabel, b)] in
      let to_uchar_body =
        Exp.let_ ~loc Nonrecursive
          [Vb.mk (Pat.var ~loc { loc; txt = "i" })
             (Exp.apply ~loc
                (Exp.ident ~loc
                   { loc; txt = (Ldot ((Lident "Char"), "code")) })
                [(Nolabel, e_c)])]
          (Exp.ifthenelse ~loc (cmp_lt e_i (Exp.constant ~loc (Const.int 0)))
             raise_undef
             (Some
                (Exp.ifthenelse ~loc
                   (cmp_gt e_i (Exp.constant ~loc (Const.int 255)))
                   raise_undef
                   (Some
                      (Exp.let_ ~loc Nonrecursive
                         [Vb.mk (Pat.var ~loc { loc; txt = "u" })
                            (Exp.apply ~loc
                               (Exp.ident ~loc
                                  {
                                    loc;
                                    txt = (Ldot ((Lident "Array"), "get"))
                                  }) [(Nolabel, e_arr); (Nolabel, e_i)])]
                         (Exp.ifthenelse ~loc
                            (cmp_lt e_u (Exp.constant ~loc (Const.int 0)))
                            raise_undef (Some e_u))))))) in
      let utf8_body =
        Exp.apply ~loc
          (Exp.ident ~loc { loc; txt = (Ldot ((Lident "UTF8"), "init")) })
          [(Nolabel,
             (Exp.apply ~loc
                (Exp.ident ~loc
                   { loc; txt = (Ldot ((Lident "String"), "length")) })
                [(Nolabel, e_s)]));
          (Nolabel,
            (Exp.fun_ ~loc Nolabel None (Pat.var ~loc { loc; txt = "i" })
               (Exp.apply ~loc
                  (Exp.ident ~loc { loc; txt = (Lident "to_uchar") })
                  [(Nolabel,
                     (Exp.apply ~loc
                        (Exp.ident ~loc
                           { loc; txt = (Ldot ((Lident "String"), "get")) })
                        [(Nolabel, e_s);
                        (Nolabel,
                          (Exp.apply ~loc
                             (Exp.ident ~loc { loc; txt = (Lident "-") })
                             [(Nolabel,
                                (Exp.ident ~loc { loc; txt = (Lident "i") }));
                             (Nolabel, (Exp.constant ~loc (Const.int 1)))]))]))])))] in
      let utf16_body =
        Exp.apply ~loc
          (Exp.ident ~loc { loc; txt = (Ldot ((Lident "UTF16"), "init")) })
          [(Nolabel,
             (Exp.apply ~loc
                (Exp.ident ~loc
                   { loc; txt = (Ldot ((Lident "String"), "length")) })
                [(Nolabel, e_s)]));
          (Nolabel,
            (Exp.fun_ ~loc Nolabel None (Pat.var ~loc { loc; txt = "i" })
               (Exp.apply ~loc
                  (Exp.ident ~loc { loc; txt = (Lident "to_uchar") })
                  [(Nolabel,
                     (Exp.apply ~loc
                        (Exp.ident ~loc
                           { loc; txt = (Ldot ((Lident "String"), "get")) })
                        [(Nolabel, e_s);
                        (Nolabel,
                          (Exp.apply ~loc
                             (Exp.ident ~loc { loc; txt = (Lident "-") })
                             [(Nolabel,
                                (Exp.ident ~loc { loc; txt = (Lident "i") }));
                             (Nolabel, (Exp.constant ~loc (Const.int 1)))]))]))])))] in
      let utf32_body =
        Exp.apply ~loc
          (Exp.ident ~loc { loc; txt = (Ldot ((Lident "UTF32"), "init")) })
          [(Nolabel,
             (Exp.apply ~loc
                (Exp.ident ~loc
                   { loc; txt = (Ldot ((Lident "String"), "length")) })
                [(Nolabel, e_s)]));
          (Nolabel,
            (Exp.fun_ ~loc Nolabel None (Pat.var ~loc { loc; txt = "i" })
               (Exp.apply ~loc
                  (Exp.ident ~loc { loc; txt = (Lident "to_uchar") })
                  [(Nolabel,
                     (Exp.apply ~loc
                        (Exp.ident ~loc
                           { loc; txt = (Ldot ((Lident "String"), "get")) })
                        [(Nolabel, e_s);
                        (Nolabel,
                          (Exp.apply ~loc
                             (Exp.ident ~loc { loc; txt = (Lident "-") })
                             [(Nolabel,
                                (Exp.ident ~loc { loc; txt = (Lident "i") }));
                             (Nolabel, (Exp.constant ~loc (Const.int 1)))]))]))])))] in
      [Str.exception_ ~loc
         (Te.mk_exception ~loc (Te.decl ~loc { loc; txt = "Undefined" }));
      Str.value ~loc Nonrecursive
        [Vb.mk
           (Pat.constraint_ ~loc
              (Pat.var ~loc { loc; txt = "conversion_array" }) ty_int_array)
           conversion_array_expr];
      Str.value ~loc Nonrecursive
        [Vb.mk
           (Pat.constraint_ ~loc (Pat.var ~loc { loc; txt = "to_uchar" })
              (Typ.arrow ~loc Nolabel ty_char ty_uchar))
           (Exp.fun_ ~loc Nolabel None (Pat.var ~loc { loc; txt = "c" })
              to_uchar_body)];
      Str.value ~loc Nonrecursive
        [Vb.mk (Pat.var ~loc { loc; txt = "to_utf8" })
           (Exp.fun_ ~loc Nolabel None (Pat.var ~loc { loc; txt = "s" })
              utf8_body)];
      Str.value ~loc Nonrecursive
        [Vb.mk (Pat.var ~loc { loc; txt = "to_utf16" })
           (Exp.fun_ ~loc Nolabel None (Pat.var ~loc { loc; txt = "s" })
              utf16_body)];
      Str.value ~loc Nonrecursive
        [Vb.mk (Pat.var ~loc { loc; txt = "to_utf32" })
           (Exp.fun_ ~loc Nolabel None (Pat.var ~loc { loc; txt = "s" })
              utf32_body)]]
    let mappings =
      Earley_core.Earley.fsequence
        (Earley_core.Earley.apply (fun f -> f [])
           (Earley_core.Earley.fixpoint' (fun l -> l) mapping
              (fun x -> fun f -> fun l -> f (List.cons x l))))
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.string "\n\026" "\n\026")))
           (Earley_core.Earley.fsequence_ignore (Earley_core.Earley.eof ())
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun _default_0 -> fun ms -> build_file _loc ms))))
    let entry_points = (".TXT", (Implementation (mappings, blank))) ::
      entry_points
  end
module PatolineDefault = (Pa_ocaml.Make)((Ext)(Pa_ocaml_prelude.Initial))
module M = (Pa_main.Start)(PatolineDefault)
