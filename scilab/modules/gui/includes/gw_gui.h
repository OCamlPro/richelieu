/*
 * Scilab (http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) 2006 - INRIA - Allan CORNET
 * Copyright (C) 2008 - INRIA - Vincent COUVERT
 *
 * This file must be used under the terms of the CeCILL.
 * This source file is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at
 * http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

#ifndef __GW_GUI__
#define __GW_GUI__
#include "dynlib_gui.h"
/*--------------------------------------------------------------------------*/
GUI_IMPEXP int gw_gui(void);
/*--------------------------------------------------------------------------*/
GUI_IMPEXP int sci_x_dialog(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_x_choose(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_x_mdialog(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_x_choice(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_delmenu(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_setmenu(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_unsetmenu(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_uigetdir(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_getlookandfeel(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_getinstalledlookandfeels(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_setlookandfeel(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_ClipBoard(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_toolbar(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_uicontrol(char* fname, void *pvApiCtx);
GUI_IMPEXP int sci_uimenu(char* fname, void *pvApiCtx);
GUI_IMPEXP int sci_x_choose_modeless(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_messagebox(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_waitbar(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_progressionbar(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_uigetfont(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_uigetcolor(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_getcallbackobject(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_uicontextmenu(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_uiwait(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_printfigure(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_exportUI(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_printsetupbox(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_toprint(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_uigetfile(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_usecanvas(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_displaytree(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_uiputfile(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_raise_window (char* fname, void* pvApiCtx);

// YaSp
GUI_IMPEXP int sci_about(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_mpopup(char* fname, void* pvApiCtx);
GUI_IMPEXP int sci_fire_closing_finished (char* fname, void* pvApiCtx);

/*--------------------------------------------------------------------------*/
#endif /*  __GW_GUI__ */
/*--------------------------------------------------------------------------*/

