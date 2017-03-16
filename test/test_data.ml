(* expect results from parsing test.idl *)
open Webidl

let check_int_ext = [
  `Ident_list (("Exposed", ["Window"]));
  `Ident_list (("Exposed", ["Window"; "Worker"]));
]

let check_stringifier = { 
  Data.ident = "check_stringifier"; inheritance = None;
  interface_members =
    [([],
      `Stringifier (`Attribute ({ Data.is_static = false;
                                  is_readonly = false;
                                  is_inherit = false;
                                  type_ = `Domstring;
                                  name = "ident1"})));
     ([],
      `Stringifier (`Operation ({ Data.specials = [];
                                  is_static = false;
                                  type_ = `Domstring;
                                  ident = (Some "ident2");
                                  arguments = [] })));
     ([],
      `Stringifier (`Operation ({ Data.specials = [];
                                  is_static = false;
                                  type_ = `Domstring;
                                  ident = None; arguments = [] })));
     ([], `Stringifier (`None))]
}

let check_serializer = { 
  Data.ident = "check_serializer"; inheritance = None;
  interface_members =
    [([], `Serializer (`None));
     ([], `Serializer (`Ident ("test")));
     ([], `Serializer (`Pattern_map (`Inherit (["a"; "b"; "c"]))));
     ([], `Serializer (`Pattern_map (`Identifiers (["d"; "e"; "f"]))));
     ([], `Serializer (`Pattern_map (`Getter)));
     ([], `Serializer (`Pattern_list (`Getter)));
     ([], `Serializer (`Pattern_list (`Identifiers (["g"; "h"; "i"]))))
    ]
}

let check_attribute = { 
  Data.ident = "check_attribute"; inheritance = None;
  interface_members =
    [([], `Attribute ({ Data.is_static = false; is_readonly = true;
                        is_inherit = false; type_ = `Domstring;
                        name = "name1"}));
     ([], `Attribute ({ Data.is_static = true; is_readonly = true;
                        is_inherit = false; type_ = `Domstring;
                        name = "name2"}));
     ([], `Attribute ({ Data.is_static = true; is_readonly = false;
                        is_inherit = false; type_ = `Domstring;
                        name = "name3"}));
     ([], `Attribute ({ Data.is_static = false;
                        is_readonly = false; is_inherit = true;
                        type_ = `Domstring; name = "name4"
                      }));
     ([], `Attribute ({ Data.is_static = false;
                        is_readonly = false; is_inherit = false;
                        type_ = `Ident ("Test");
                        name = "required"}))
    ]
}

let check_operation = { 
  Data.ident = "check_operation";
  inheritance = (Some "Parent");
  interface_members =
    [([],
      `Operation ({ Data.specials = []; is_static = false;
                    type_ = `Boolean;
                    ident = (Some "isMouseOver"); arguments = []
                  }));
     ([],
      `Operation ({ Data.specials = [`Getter];
                    is_static = false; type_ = `Any;
                    ident = (Some "test"); arguments = [] }));
     ([],
      `Operation ({ Data.specials = []; is_static = false;
                    type_ = `Object; ident = (Some "f");
                    arguments =
                      [([],
                        { Data.type_ = `Domstring;
                          name = "c";
                          necessity =
                            `Optional ((Some (`String ("value")))) })
                      ]
                  }));
     ([],
      `Operation ({ Data.specials = []; is_static = false;
                    type_ = `Void; ident = (Some "g");
                    arguments =
                      [([],
                        { Data.type_ = `Ident ("Event");
                          name = "a";
                          necessity = `Required (`Fixed) });
                       ([],
                        { Data.type_ = `Nullable (`Domstring);
                          name = "b";
                          necessity = `Required (`Fixed) });
                       ([],
                        { Data.type_ = `Domstring;
                          name = "c";
                          necessity = `Optional (None) });
                       ([],
                        { Data.type_ = `Double;
                          name = "d";
                          necessity = `Required (`Variadic) })
                      ]
                  }));
     ([],
      `Operation ({ Data.specials = []; is_static = true;
                    type_ = `Void; ident = (Some "h");
                    arguments =
                      [([`Ident_list (("PutForwards", ["name"]))],
                        { Data.type_ = `Ident ("Event");
                          name = "maplike";
                          necessity = `Required (`Fixed) })
                      ]
                  }));
     ([],
      `Operation ({ Data.specials = [`Setter];
                    is_static = false; type_ = `Float;
                    ident = None; arguments = [] }))
    ]
}

let check_others = { 
  Data.ident = "check_others"; inheritance = None;
  interface_members =
    [([], `Iterable ((`Domstring, None)));
     ([], `Iterable ((`Domstring, (Some `Float))));
     ([], `Maplike ({ Data.is_readonly = true;
                      key_type = `Domstring;
                      value_type = `Float }));
     ([], `Maplike ({ Data.is_readonly = false;
                      key_type = `Domstring;
                      value_type = `Float }));
     ([], `Setlike ({ Data.is_readonly = true;
                      key_type = `Domstring }));
     ([], `Setlike ({ Data.is_readonly = false;
                      key_type = `Domstring }))
    ]
}

let check_namespace = { 
  Data.ident = "check_namespace";
  namespace_members =
    [([],
      `Attribute ({ Data.is_static = false; is_readonly = true;
                    is_inherit = false;
                    type_ = `Ident ("Vector");
                    name = "unit"}));
     ([],
      `Operation ({ Data.specials = []; is_static = false;
                    type_ = `Double;
                    ident = (Some "dotProduct");
                    arguments =
                      [([],
                        { Data.type_ = `Ident ("Vector");
                          name = "x";
                          necessity = `Required (`Fixed) });
                       ([],
                        { Data.type_ = `Ident ("Vector");
                          name = "y";
                          necessity = `Required (`Fixed) })
                      ]
                  }));
     ([],
      `Operation ({ Data.specials = []; is_static = false;
                    type_ = `Ident ("Vector");
                    ident = (Some "crossProduct");
                    arguments =
                      [([],
                        { Data.type_ = `Ident ("Vector");
                          name = "x";
                          necessity = `Required (`Fixed) });
                       ([],
                        { Data.type_ = `Ident ("Vector");
                          name = "y";
                          necessity = `Required (`Fixed) })
                      ]
                  }))
    ]
}

let check_dictionary = {
  Data.ident = "check_dictionary"; inheritance = None;
  dictionary_members =
    [([],
      { Data.is_required = false; type_ = `Domstring;
        ident = "identifier";
        default = (Some (`String ("value"))) });
     ([],
      { Data.is_required = true; type_ = `Long; ident = "req";
        default = None })
    ]
}

let check_ext = { 
  Data.ident = "check_ext"; inheritance = None;
  interface_members =
    [([`Argument_list (("Replaceable", []))],
      `Const ((`Short, "test1", `Int (1))));
     ([`Argument_list (("Constructor",
                        [([],
                          { Data.type_ = `Double;
                            name = "x";
                            necessity =
                              `Required (`Fixed) });
                         ([],
                          { Data.type_ = `Double;
                            name = "y";
                            necessity =
                              `Required (`Fixed) })
                        ]))
      ],
      `Const ((`Short, "test2", `Int (1))));
     ([`Named_arg_list (("NamedConstructor", "Image",
                         [([],
                           { Data.type_ = `Domstring;
                             name = "src";
                             necessity =
                               `Required (`Fixed) })
                         ]))
      ],
      `Operation ({ Data.specials = [];
                    is_static = false;
                    type_ = `Unsigned (`Short);
                    ident = (Some "test3");
                    arguments =
                      [([],
                        { Data.type_ = `Ident ("Node");
                          name = "node";
                          necessity = `Required (`Fixed)
                        })
                      ]
                  }));
     ([`Ident_list (("PutForwards", ["name"]))],
      `Stringifier (`None));
     ([`Ident_list (("Exposed", ["Window"; "Worker"]))
      ],
      `Operation ({ Data.specials = [`Getter];
                    is_static = false;
                    type_ = `Double; ident = None;
                    arguments = [] }))
    ]
}

let check_callback = (
  "check_callback", `Void,
  [([],
    { Data.type_ = `Domstring; name = "a";
      necessity = `Required (`Fixed) })
  ])

let check_enum = ("check_enum", ["a"; "b"; "c"])

let check_typedef = (`Sequence (`Ident ("Sq")), "Seq")

let check_implements = ("Implements_test1", "Implements_test2")