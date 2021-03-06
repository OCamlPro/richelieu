// =============================================================================
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 2008 - DIGITEO - Allan CORNET
//
//  This file is distributed under the same license as the Scilab package.
// =============================================================================

// <-- ENGLISH IMPOSED -->

// <-- Non-regression test for bug 3630 -->
//
// <-- Bugzilla URL -->
// http://bugzilla.scilab.org/show_bug.cgi?id=3630
//
// <-- Short Description -->
// functions added by addinter are not visible by 'what'

ilib_verbose(0);

test_path = get_absolute_file_path('bug_3630.tst');

currentpath = pwd();

cd TMPDIR;
cd ../;
OS_TMP_DIR = pwd();


mkdir(OS_TMP_DIR,'bug_3630');
TEST_DIR = OS_TMP_DIR + filesep() + 'bug_3630';

copyfile(SCI+'/modules/dynamic_link/tests/nonreg_tests/bug_3630.c' , TEST_DIR + filesep() + 'bug_3630.c');

chdir(TEST_DIR);

files=['bug_3630.c'];
ilib_build('libc_fun',['c_sum','c_intsum';'c_sub','c_intsub'],files,[]);

[primitives1,commandes] = what();
nbprimitives1 = size(primitives1,'*');

// load the shared library 
exec loader.sce;

chdir(currentpath);

[primitives2,commandes] = what();
nbprimitives2 = size(primitives2,'*');

if (nbprimitives2 - nbprimitives1) <> 2 then pause,end

if ~or(primitives2 == 'c_sum') then pause,end
if ~or(primitives2 == 'c_sub') then pause,end

// ulink() all libraries
ulink();
clearfun('c_sum');
clearfun('c_sub');

[primitives3,commandes] = what();
nbprimitives3 = size(primitives3,'*');

if or(primitives3 == 'c_sum') then pause,end
if or(primitives3 == 'c_sub') then pause,end

if (nbprimitives3 - nbprimitives1) <> 0 then pause,end

//remove TMP_DIR
rmdir(TEST_DIR,'s');
// =============================================================================
