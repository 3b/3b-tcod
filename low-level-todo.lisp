
(defcfun ("TCOD_lex_delete" lex-delete) :void
  (a (:pointer :void)))

(defcfun ("TCOD_lex_expect_token_type" lex-expect-token-type) :bool
  (a (:pointer :void))
  (a :int))

(defcfun ("TCOD_lex_expect_token_value" lex-expect-token-value) :bool
  (a (:pointer :void))
  (a :int)
  (a (:pointer :char)))

(defcfun ("TCOD_lex_get_last_javadoc" lex-get-last-javadoc) (:pointer :char)
  (a (:pointer :void)))

(defcfun ("TCOD_lex_get_token_name" lex-get-token-name) (:pointer :char)
  (a :int))

(defcfun ("TCOD_lex_hextoint" lex-hextoint) :int
  (a :char))

(defcfun ("TCOD_lex_new" lex-new) (:pointer :void)
  (a (:pointer (:pointer :char)))
  (a (:pointer (:pointer :char)))
  (a (:pointer :char)))

(defcfun ("TCOD_lex_new_intern" lex-new-intern) (:pointer :void))

(defcfun ("TCOD_lex_parse" lex-parse) :int
  (a (:pointer :void)))

(defcfun ("TCOD_lex_parse_until_token_type" lex-parse-until-token-type) :int
  (a (:pointer :void))
  (a :int))

(defcfun ("TCOD_lex_parse_until_token_value" lex-parse-until-token-value) :int
  (a (:pointer :void))
  (a (:pointer :char)))

(defcfun ("TCOD_lex_restore" lex-restore) :void
  (a (:pointer :void))
  (a (:pointer :void)))

(defcfun ("TCOD_lex_savepoint" lex-savepoint) :void
  (a (:pointer :void))
  (a (:pointer :void)))

(defcfun ("TCOD_lex_set_data_buffer" lex-set-data-buffer) :void
  (a (:pointer :void))
  (a (:pointer :char)))

(defcfun ("TCOD_lex_set_data_file" lex-set-data-file) :bool
  (a (:pointer :void))
  (a (:pointer :char)))


(defcfun ("TCOD_tree_add_son" tree-add-son) :void
  (a (:pointer :void))
  (a (:pointer :void)))

(defcfun ("TCOD_tree_new" tree-new) (:pointer :void))


(defctype zip* :pointer)
(defcfun ("TCOD_zip_new" zip-new) zip*)

(defcfun ("TCOD_zip_delete" zip-delete) :void
  (a (:pointer :void)))

(defcfun ("TCOD_zip_get_char" zip-get-char) :char
  (a (:pointer :void)))

(defcfun ("TCOD_zip_get_color" zip-get-color) :int
  (a (:pointer :void)))

(defcfun ("TCOD_zip_get_console" zip-get-console) (:pointer :void)
  (a (:pointer :void)))

(defcfun ("TCOD_zip_get_current_bytes" zip-get-current-bytes) :int
  (a (:pointer :void)))

(defcfun ("TCOD_zip_get_data" zip-get-data) :int
  (a (:pointer :void))
  (a :int)
  (a (:pointer :void)))

(defcfun ("TCOD_zip_get_float" zip-get-float) :float
  (a (:pointer :void)))

(defcfun ("TCOD_zip_get_image" zip-get-image) (:pointer :void)
  (a (:pointer :void)))

(defcfun ("TCOD_zip_get_int" zip-get-int) :int
  (a (:pointer :void)))

(defcfun ("TCOD_zip_get_remaining_bytes" zip-get-remaining-bytes) :int
  (a (:pointer :void)))

(defcfun ("TCOD_zip_get_string" zip-get-string) (:pointer :char)
  (a (:pointer :void)))

(defcfun ("TCOD_zip_load_from_file" zip-load-from-file) :int
  (a (:pointer :void))
  (a (:pointer :char)))


(defcfun ("TCOD_zip_put_char" zip-put-char) :void
  (a (:pointer :void))
  (a :char))

(defcfun ("TCOD_zip_put_color" zip-put-color) :void
  (a (:pointer :void))
  (a :int))

(defcfun ("TCOD_zip_put_console" zip-put-console) :void
  (a (:pointer :void))
  (a (:pointer :void)))

(defcfun ("TCOD_zip_put_data" zip-put-data) :void
  (a (:pointer :void))
  (a :int)
  (a (:pointer :void)))

(defcfun ("TCOD_zip_put_float" zip-put-float) :void
  (a (:pointer :void))
  (a :float))

(defcfun ("TCOD_zip_put_image" zip-put-image) :void
  (a (:pointer :void))
  (a (:pointer :void)))

(defcfun ("TCOD_zip_put_int" zip-put-int) :void
  (a (:pointer :void))
  (a :int))

(defcfun ("TCOD_zip_put_string" zip-put-string) :void
  (a (:pointer :void))
  (a (:pointer :char)))

(defcfun ("TCOD_zip_save_to_file" zip-save-to-file) :int
  (a (:pointer :void))
  (a (:pointer :char)))

(defcfun ("TCOD_zip_skip_bytes" zip-skip-bytes) :void
  (a (:pointer :void))
  (a :int))
