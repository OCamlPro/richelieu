/*
 * Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 * Copyright (C) 2007 - INRIA
 * Copyright (C) 2009 - DIGITEO - Allan CORNET
 * 
 * This file must be used under the terms of the CeCILL.
 * This source file is licensed as described in the file COPYING, which
 * you should have received as part of this distribution.  The terms
 * are also available at    
 * http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */
/*--------------------------------------------------------------------------*/
#include "filemanager.hxx"

extern "C"
{
#include "mclose.h"
#include "filesmanagement.h"
#include "delfile.h"
#include "core_math.h" /* Min Max */
#include "sciprint.h"
#include "localization.h"
#include "sci_warning.h"
#include "charEncoding.h"
}
/*--------------------------------------------------------------------------*/
int mcloseCurrentFile()
{
    return mclose(FileManager::getCurrentFile());
}

int mcloseAll()
{
    int iFileCount = FileManager::getFileMaxID();
    for(int i = iFileCount - 1 ; i >= 0 ; i--)
    {
        switch (i)
        {
        case 0: // stderr
        case 5: // stdin
        case 6: // stdout
            continue;
            break;
        default :
            if(FileManager::getFile(i) != NULL)
            {//call mclose only for existing opened files.
                int iRet = mclose(i);
                if(iRet)
                {
                    return iRet;
                }
            }
        }
    }

    return 0;
}

int mclose(int _iID)
{
    File* pF = FileManager::getFile(_iID);
    if(pF != NULL)
    {
        int iRet = fclose(pF->getFiledesc());

        // this function previously called ferror on a just before fclosed FILE* that could lead to crash at exit, depending on libc implementation.
        if(iRet != 0)
        {
            return 1;
        }

        FileManager::deleteFile(_iID);
    }
    else
    {
        if (getWarningMode()) 
        {
            sciprint(_("%s: Cannot close file whose descriptor is %d: File is not active.\n"), "mclose", _iID);
        }
    }
    return 0;
}

void C2F(mclose) (int *fd, double *res)
{   
	int fd1 = -1;
	*res = 0.0;
	
	switch ( *fd )
	{
		case ALL_FILES_DESCRIPTOR :
		/* closing all opened files */
		for ( fd1=0; fd1< GetMaximumFileOpenedInScilab(); fd1++) 
		{
			FILE* stream=GetFileOpenedInScilab(fd1) ;
			if ( stream )
			{
				int res1 = 1;
				res1=fclose( stream );
				// this function previously called ferror on a just before fclosed FILE* that could lead to crash at exit, depending on libc implementation.
				if (res1 != 0) *res =1;
				C2F(delfile)(&fd1);
				/* bug 3897 */
				/* initialize file ID */
				SetCurrentFileId(-1);
			}
		}
		break;

		default :
		{	
			fd1 = (*fd == -1 ) ? GetCurrentFileId() : Min(Max(*fd,0),GetMaximumFileOpenedInScilab()-1);
			if ( fd1 != -1 ) 
			{
				if ( GetFileOpenedInScilab(fd1) )
				{
					int prevId = -1;

					if (fclose(GetFileOpenedInScilab(fd1)))
					{
						*res = (double)ferror(GetFileOpenedInScilab(fd1));
					}
					C2F(delfile)(&fd1);

					/* bug 3897 */
					/* set as current file previous opened file if exists */
					prevId = GetPreviousFileId();

					if ( GetFileOpenedInScilab(prevId) )
					{
						SetCurrentFileId(prevId);
					}
				}
				else
				{
					*res = 0.0;
					if (getWarningMode()) 
					{
						sciprint(_("%s: Cannot close file whose descriptor is %d: File is not active.\n"),"mclose",fd1);
					}
				}
			}
			else 
			{
				*res = -1.0;
				if (getWarningMode()) 
				{
					sciprint(_("%s: Cannot close file whose descriptor is %d: No file to close.\n"),"mclose",fd1);
				}
			}
		}
	}
}
/*--------------------------------------------------------------------------*/
