/*
*  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
*  Copyright (C) 2009-2009 - DIGITEO - Bruno JOFRET
*  Copyright (C) 2010 - DIGITEO - Antoine ELIAS
*  Copyright (C) 2010 - DIGITEO - Allan CORNET
*
*  This file must be used under the terms of the CeCILL.
*  This source file is licensed as described in the file COPYING, which
*  you should have received as part of this distribution.  The terms
*  are also available at
*  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
*
*/
/*--------------------------------------------------------------------------*/ 
#include "dynhdf5.hxx"

extern "C"
{
#include <string.h>
#include "h5_fileManagement.h"
#include "FileExist.h"
#include "deleteafile.h"
#include "isdir.h"
#include "splitpath.h"
#include "scicurdir.h"
#include "MALLOC.h"
#include "os_strdup.h"
}
/*--------------------------------------------------------------------------*/ 
static char *getPathFilename(char *fullfilename);
static char *getFilenameWithExtension(char *fullfilename);
/*--------------------------------------------------------------------------*/ 
int createHDF5File(char *name) 
{
    hid_t       file;
    hid_t fapl = DynHDF5::dynH5Pcreate(H5P_FILE_ACCESS);
    char *pathdest = getPathFilename(name);
    char *currentpath = NULL;
    char *filename = getFilenameWithExtension(name);
    int ierr = 0;

    //H5Pset_fclose_degree(fapl, H5F_CLOSE_STRONG);

    /* TO DO : remove when HDF5 will be fixed ... */
    /* HDF5 does not manage no ANSI characters */
    /* UGLY workaround :( */
    /* We split path, move in this path, open file */
    /* and return in previous place */
    /* see BUG 6440 */
    currentpath = scigetcwd(&ierr);

    //prevent error msg to change directory to ""
    if (strcmp(pathdest, "") != 0)
    {
        scichdir(pathdest);
    }

    FREE(pathdest);
    /*bug 5629 : to prevent replace directory by file*/
    if (isdir(filename))
    {
        FREE(filename);
        FREE(currentpath);
        return -2;
    }

    if (FileExist(filename))
    {
        deleteafile(filename);
    }
    /*
    * Create a new file using the default properties.
    */

    file = DynHDF5::dynH5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    scichdir(currentpath);

    if (currentpath) {FREE(currentpath); currentpath = NULL;}
    if (filename) {FREE(filename); filename = NULL;}
    if (pathdest) {FREE(pathdest); pathdest = NULL;}

    return file;
}
/*--------------------------------------------------------------------------*/ 
int openHDF5File(char *name) 
{
    hid_t           file;
    char *pathdest = getPathFilename(name);
    char *currentpath = NULL;
    char *filename = getFilenameWithExtension(name);
    int ierr = 0;

    /* TO DO : remove when HDF5 will be fixed ... */
    /* HDF5 does not manage no ANSI characters */
    /* UGLY workaround :( */
    /* We split path, move in this path, open file */
    /* and return in previous place */
    /* see BUG 6440 */
    currentpath = scigetcwd(&ierr);

    //prevent error msg to change directory to ""
    if(strcmp(pathdest, "") != 0)
    {
        scichdir(pathdest);
    }

    file = DynHDF5::dynH5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);

    scichdir(currentpath);

    if (currentpath) {FREE(currentpath); currentpath = NULL;}
    if (filename) {FREE(filename); filename = NULL;}
    if (pathdest) {FREE(pathdest); pathdest = NULL;}

    return file;
}
/*--------------------------------------------------------------------------*/
int isHDF5File(char* _pstFilename)
{
    int iRet = 0;
    char *pathdest = getPathFilename(_pstFilename);
    char *currentpath = NULL;
    char *filename = getFilenameWithExtension(_pstFilename);
    int ierr = 0;

    /* TO DO : remove when HDF5 will be fixed ... */
    /* HDF5 does not manage no ANSI characters */
    /* UGLY workaround :( */
    /* We split path, move in this path, open file */
    /* and return in previous place */
    /* see BUG 6440 */
    currentpath = scigetcwd(&ierr);
    {

        //prevent error msg to change directory to ""
        if (strcmp(pathdest, "") != 0)
        {
            scichdir(pathdest);
        }
        FREE(pathdest);

        iRet = H5Fis_hdf5(filename);
        FREE(filename);
    }
    scichdir(currentpath);
    FREE(currentpath);

    return iRet > 0 ? 1 : 0;
}

void closeHDF5File(int file)
{
    herr_t status					= 0;

    //	H5Fflush(file, H5F_SCOPE_GLOBAL);
    status =  DynHDF5::dynH5Fclose(file);
    if (status < 0)
    {
        fprintf(stderr, "%s", "failed to close file");
    }
    DynHDF5::dynCLoseLib();
}
/*--------------------------------------------------------------------------*/ 
static char *getPathFilename(char *fullfilename)
{
    char *path = NULL;
    if (fullfilename)
    {
        char* drv  = os_strdup(fullfilename);
        char* dir  = os_strdup(fullfilename);
        char* name = os_strdup(fullfilename);
        char* ext  = os_strdup(fullfilename);

        path = os_strdup(fullfilename);

        if (drv && dir && name && ext && path)
        {
            splitpath(fullfilename, FALSE, drv, dir, name, ext);

            if (strcmp(drv, "") == 0)
            {
                strcpy(path, dir);
            }
            else
            {
                strcpy(path, drv);
                strcat(path, dir);
            }
        }

        if (drv) {FREE(drv); drv = NULL;}
        if (dir) {FREE(dir); dir = NULL;}
        if (name) {FREE(name); name = NULL;}
        if (ext) {FREE(ext); ext = NULL;}
    }
    return path;
}
/*--------------------------------------------------------------------------*/ 
static char *getFilenameWithExtension(char *fullfilename)
{
    char *filename = NULL;
    if (fullfilename)
    {
        char* drv  = os_strdup(fullfilename);
        char* dir  = os_strdup(fullfilename);
        char* name = os_strdup(fullfilename);
        char* ext  = os_strdup(fullfilename);

        filename = os_strdup(fullfilename);

        if (drv && dir && name && ext && filename)
        {
            splitpath(fullfilename, FALSE, drv, dir, name, ext);

            if (strcmp(ext, "") == 0)
            {
                strcpy(filename, name);
            }
            else
            {
                strcpy(filename, name);
                strcat(filename, ext);
            }
        }

        if (drv) {FREE(drv); drv = NULL;}
        if (dir) {FREE(dir); dir = NULL;}
        if (name) {FREE(name); name = NULL;}
        if (ext) {FREE(ext); ext = NULL;}
    }
    return filename;
}
/*--------------------------------------------------------------------------*/ 

