#!/usr/bin/python
# -*- coding: utf-8 -*-
# __author__ = "PengZhang"

import os

sbatch_job_id_all = "/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/code/20170406/breast_test/all_time.txt"
sbatch_name_file = "/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/code/20170406/breast_test/"
with open("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/code/20170406/breast_test/all_time.txt","r") as tmp_file:
	for text in tmp_file.readlines():
		text = text.strip()
		print text
		os.system("scontrol show job %s |grep JobState"%text)
		os.system("scontrol show job %s |grep RunTime"%text)
		print "cd %s"%(sbatch_name_file)
		print "less %sgrep%s.out"%(sbatch_name_file,text)
		print "less %sgrep%s.err"%(sbatch_name_file,text)
		print "scancel %s"%text
