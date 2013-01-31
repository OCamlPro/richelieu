/*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2012-2012 - OCAMLPRO INRIA - Fabrice LE FESSANT
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

#include <stdlib.h>
#include <stdio.h>

extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <stdlib.h>
}
#include "scicaml.hxx"
#include "ocpsci.hxx"

extern "C" {
  extern void caml_startup(char** argv);
  value scicaml_register_callback_c(value callback);
  value scicaml_get_double_c(value s_v, value pos_v);
  value scicaml_set_double_c(value s_v, value pos_v, value d_v);
}

static value scicaml_registered_callback = Val_unit;

value scicaml_register_callback_c(value callback)
{
  scicaml_registered_callback = callback;
  caml_register_global_root(&scicaml_registered_callback);
  return Val_unit;
}

/*
char* scicaml_analyze(char *buf)
{
  value ret;

  if( scicaml_registered_callback == Val_unit ){
    char *argv[1];
    argv[0] = NULL;
    caml_startup(argv);
  }
  ret = caml_callback2(scicaml_registered_callback, (value)buf, Val_int(3));
  return buf;
}
*/

value scicaml_get_double_c(value s_v, value pos_v)
{
  char *s = (char*)s_v;
  int pos = Int_val(pos_v);
  return caml_copy_double( *(double*)(s+pos) );
}

value scicaml_set_double_c(value s_v, value pos_v, value d_v)
{
  char *s = (char*)s_v;
  int pos = Int_val(pos_v);
  double d = Double_val(d_v);

  *(double*)(s+pos) = d;
  return Val_unit;
}



int scicaml_visit(const ast::Exp &e, 
		   ast::RunVisitor *visitor, int action)
{
  int expected = visitor->expected_getSize();

  if( scicaml_registered_callback == Val_unit ){
    char *argv[1];
    argv[0] = NULL;
    caml_startup(argv);
  }
  /*  fprintf(stderr, "%d %d %d %d\n", buf[0],buf[1],buf[2],buf[3]); */
  char *buf = scicaml_ast2string(&e);
  value ret_v = caml_callback3(scicaml_registered_callback, (value)buf, 
		       Val_int(action), Val_int(expected));
  free(buf);

  if( ret_v == Val_int(0) ){ // None => an error !
    return 1;
  } else {
    ret_v = Field( ret_v, 0 );
    int nresults = Wosize_val(ret_v);
    
    if( nresults == 1 ){
      visitor->result_set( Scilab_val(Field(ret_v, 0) ) );
    } else
      if( nresults > 0 ){
	for(int i=0; i<nresults; i++){
	  visitor->result_set(i, Scilab_val(Field(ret_v, i) ) );
	}
      }  
  }
  // 1 has return means that we want to execute the visitprivate()
  return 1;
}
