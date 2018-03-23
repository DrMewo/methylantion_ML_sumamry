#!/usr/bin/python
# -*- coding: utf-8 -*-
# __author__ = "YourFather"

import os
import string
import argparse
import shutil
import json
import time
import subprocess
import sys
import time




para_data = []
for s in open(args['para_file']).readlines():
    s = s.strip().split(';')
    if not s:
        continue
    tmp_dict = dict()
    for data in s:
        position = data.find(":")
        tmp_dict[data[:position]] = data[position+1 : len(data)]
    para_data += [tmp_dict]
para_num = len(para_data[0])
std_option = dict()
file_name = args['id']
date = time.strftime('%Y%m%d',time.localtime(time.time()))
date_time = time.strftime('%Y%m%d%H%M', time.localtime(time.time()))
sbatch_name_file = args["output_dir"] + '/' + date + '/'+ file_name +'/'
sbatch_name_file_one = args["output_dir"] + '/' + date + '/'
log_file = "/mnt/ilustre/users/sanger-dev/workspace/%s/Single_%s" %(date, args['id'])

if not os.path.exists(sbatch_name_file_one):
    os.mkdir(sbatch_name_file_one)

if sbatch_name_file and not os.path.exists(sbatch_name_file):
    os.mkdir(sbatch_name_file)

sbatch_job_id_all = sbatch_name_file + 'all_time.txt'
sbatch_job_id_one = sbatch_name_file + 'one_time.txt'
#if os.path.isfile(sbatch_job_id_one):
    #os.remove(sbatch_job_id_one)
    #os.mknod(sbatch_job_id_one)
if not os.path.isfile(sbatch_job_id_all):
    os.mknod(sbatch_job_id_all)
else:
    os.remove(sbatch_job_id_all)
    os.mknod(sbatch_job_id_all)

for i in range(len(para_data)):
    sbatch_name = args["output_dir"] + '/' + date + '/' + file_name + '/test_single_' + file_name + '_' + date_time + '_' + str(i+1) + '.sbatch'
    with open(sbatch_name, "w") as tmp_file:
        tmp_file.write("#!/bin/bash\n")
        tmp_file.write("#SBATCH -c %s\n"%args["cpu"])
        tmp_file.write("#SBATCH -D %s\n"%sbatch_name_file)
        tmp_file.write("#SBATCH -n %s\n"%args["node"])
        tmp_file.write("#SBATCH -N 1\n")
        tmp_file.write("#SBATCH -J %s\n"%args["id"])
        tmp_file.write("#SBATCH -t %s-00:00\n" %args["time"])
        tmp_file.write("#SBATCH -p SANGERDEV\n")
        tmp_file.write("#SBATCH --men=100G\n")
        tmp_file.write("#SBATCH -o %sgrep%%j.out\n"%sbatch_name_file)
        tmp_file.write("#SBATCH -e %sgrep%%j.err\n"%sbatch_name_file)
        tmp_file.write("\n")
        tmp_file.write("\n")
        tmp_file.write("cd %s\n"%sbatch_name_file)
        tmp_file.write(". /mnt/ilustre/users/sanger-dev/bash_profile\n")
        tmp_file.write("export PATH=/mnt/ilustre/users/sanger-dev/app/gcc/5.1.0/bin:$PATH\n")
        tmp_file.write("export LD_LIBRARY_PATH=/mnt/ilustre/users/sanger-dev/app/gcc/5.1.0/lib64:$LD_LIBRARY_PATH\n")
        tmp_file.write("export PERLBREW_ROOT=/mnt/ilustre/users/sanger-dev/app/program/perl\n")
        tmp_file.write("export PATH=/mnt/ilustre/users/sanger-dev/app/program/perl/perls/perl-5.24.0/bin:$PATH\n")
        para_num = len(para_data[i])
        for j in range(para_num):
            tmp_file.write("%s\n"%para_data[i][str(j+1)])
    os.system("sbatch %s |grep -o \"[0-9]\\{6\\}\" > %s" %(sbatch_name, sbatch_job_id_one))
    with open(sbatch_job_id_one, "r") as a:
        text = a.read()
    with open(sbatch_job_id_all, "r") as all:
        text_all = all.read()
    os.remove(sbatch_job_id_all)
    with open(sbatch_job_id_all, "w") as allnew:
        allnew.write("%s%s"%(text_all,text))

py_name = args["output_dir"] + '/' + date +'/' + file_name + '/stat_sbatch.py'
with open(py_name, "w") as stat:
    stat.write("#!/usr/bin/python\n")
    stat.write("# -*- coding: utf-8 -*-\n")
    stat.write("# __author__ = \"PengZhang\"\n")
    stat.write("\n")
    stat.write("import os\n")
    stat.write("\n")
    stat.write("sbatch_job_id_all = \"%s\"\n"%sbatch_job_id_all)
    stat.write("sbatch_name_file = \"%s\"\n"%sbatch_name_file)
    stat.write("with open(\"%s\",\"r\") as tmp_file:\n"%sbatch_job_id_all)
    stat.write("\tfor text in tmp_file.readlines():\n")
    stat.write("\t\ttext = text.strip()\n")
    stat.write("\t\tprint text\n")
    stat.write("\t\tos.system(\"scontrol show job %s |grep JobState\"%text)\n")
    stat.write("\t\tprint \"cd %s\"%(sbatch_name_file)\n")
    stat.write("\t\tprint \"less %sgrep%s.out\"%(sbatch_name_file,text)\n")
    stat.write("\t\tprint \"less %sgrep%s.err\"%(sbatch_name_file,text)\n")

py_name_time = args["output_dir"] + '/' + date_time + '_' + file_name + '_stat_sbatch.py'
#print py_name_time
with open(py_name_time, "w") as stat:
    stat.write("#!/usr/bin/python\n")
    stat.write("# -*- coding: utf-8 -*-\n")
    stat.write("# __author__ = \"PengZhang\"\n")
    stat.write("\n")
    stat.write("import os\n")
    stat.write("\n")
    stat.write("sbatch_job_id_all = \"%s\"\n"%sbatch_job_id_all)
    stat.write("sbatch_name_file = \"%s\"\n"%sbatch_name_file)
    stat.write("with open(\"%s\",\"r\") as tmp_file:\n"%sbatch_job_id_all)
    stat.write("\tfor text in tmp_file.readlines():\n")
    stat.write("\t\ttext = text.strip()\n")
    stat.write("\t\tprint text\n")
    stat.write("\t\tos.system(\"scontrol show job %s |grep JobState\"%text)\n")
    stat.write("\t\tos.system(\"scontrol show job %s |grep RunTime\"%text)\n")
    stat.write("\t\tprint \"cd %s\"%(sbatch_name_file)\n")
    stat.write("\t\tprint \"less %sgrep%s.out\"%(sbatch_name_file,text)\n")
    stat.write("\t\tprint \"less %sgrep%s.err\"%(sbatch_name_file,text)\n")
    stat.write("\t\tprint \"scancel %s\"%text\n")



time.sleep(20)

print "a"
with open(sbatch_job_id_all, "r") as tmp_file:
    for text in tmp_file.readlines():
        text = text.strip()
        print text
        os.system("scontrol show job %s |grep JobState\n"%text)
        print "cd %s"%(sbatch_name_file)
        print "less %sgrep%s.out"%(sbatch_name_file,text)
        print "less %sgrep%s.err"%(sbatch_name_file,text)
        print "\n"

        
# Sample : python test_tool_bat.py -i zp -p ./para.txt -o ./
