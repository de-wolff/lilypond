#!@PYTHON@

# mf-to-table.py -- convert spacing info in  MF logs .afm and .tex
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1997 Han-Wen Nienhuys <hanwen@cs.uu.nl>

import os
import sys
import getopt
import string
import re
import time


postfixes = ['log', 'dvi', '2602gf', 'tfm']

def read_log_file (fn):
	str = open (fn).read ()
	str = re.sub ('\n', '', str) 	
	str = re.sub ('[\t ]+', ' ', str) 

	deps = []
	autolines = []
	def include_func (match, d = deps):
		d.append (match.group (1))
		return ''

	def auto_func (match, a = autolines):
		a.append (match.group (1))
		return ''

	str = re.sub ('\(([a-zA-Z_0-9-]+\.mf)', include_func, str)
	str = re.sub ('@{(.*?)@}', auto_func, str)

	return (autolines, deps)



class Char_metric:
	def __init__ (self):
		pass


def parse_logfile (fn):
	(autolines, deps) = read_log_file (fn)
	charmetrics = []
	global_info = {}
	group = ''

	for l in autolines:
		tags = string.split(l, '@:')
		if tags[0] == 'group':
			group = tags[1]
		elif tags[0] == 'char':
			m = {
				'description':  tags[1],
				'name': group + '-' + tags[7],
				'tex': tags[8],
				'code': string.atoi (tags[2]),
				'breapth':string.atof (tags[3]),
				'width': string.atof (tags[4]),
				'depth':string.atof (tags[5]),
				'height':string.atof (tags[6])
				}
			charmetrics.append (m)
		elif tags[0] == 'font':
			global_info['FontName'] = string.join (tags[1:])
			global_info['FontFamily']=tags[1]
	
	return (global_info, charmetrics, deps)


def write_afm_char_metric(file, charmetric):

	f = 1000;
	tup = (charmetric['code'],
		charmetric['width'] + charmetric['breapth'],
		charmetric['name'],
		-charmetric['breapth'] *f,
		-charmetric['depth']*f,
		charmetric['width']*f,
		charmetric['height']*f)
	
	
	file.write ('C %d ; WX %d ; N  %s ;  B %d %d %d %d ;\n'% tup)
	
def write_afm_metric (file, global_info, charmetrics):
	file.write (r"""
StartFontMetrics 2.0
Comment Automatically generated by mf-to-table.py
""")
	for (k,v) in global_info.items():
		file.write ("%s %s\n" % (k,v))
	file.write ('StartCharMetrics %d\n' % len(charmetrics ))
	for m in charmetrics:
		write_afm_char_metric (file,m)
	file.write ('EndCharMetrics\n')
	file.write ('EndFontMetrics %d\n')


def write_tex_defs (file, global_info, charmetrics):
	nm = global_info['FontFamily']
	for m in charmetrics:
		file.write (r'''\def\%s%s{\char%d}%s''' % (nm, m['tex'], m['code'],'\n'))


def write_deps (file, deps, targets):
	for t in targets:
		file.write ('%s '% t)
	file.write (": ")
	for d in deps:
		file.write ('%s ' % d)
	file.write ('\n')

def help():
    sys.stdout.write(r"""Usage: mf-to-table [options] LOGFILEs
Generate feta metrics table from preparated feta log\n
Options:
  -a, --afm=FILE         .afm file
  -d, --dep=FILE         print dependency info to FILE
  -h, --help             print this help
  -l, --ly=FILE          name output table
  -o, --outdir=DIR       prefix for dependency info
  -p, --package=DIR      specify package
  -t, --tex=FILE         name output tex chardefs"""
)
    sys.exit (0)



(options, files) = getopt.getopt(
    sys.argv[1:], 'a:d:hl:o:p:t:', 
    ['afm=', 'outdir=', 'dep=',  'tex=', 'debug', 'help', 'package='])


texfile_nm = '';
depfile_nm = ''
afmfile_nm = ''
outdir_prefix = '.'

for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--dep' or o == '-d':
		depfile_nm = a
	elif o == '--outdir' or o == '-o':
		outdir_prefix = a
	elif o == '--tex' or o == '-t':
		texfile_nm = a
	elif o== '--help' or o == '-h':
		help()
	elif o=='--afm' or o == '-a':
		afmfile_nm = a
	elif o == '--debug':
		debug_b = 1
	elif o == '-p' or o == '--package':
		topdir = a
	else:
		print o
		raise getopt.error

for filenm in files:
	(g,m, deps) =  parse_logfile (filenm)
	afm = open (afmfile_nm, 'w')
	write_afm_metric (afm, g,m)
	write_tex_defs (open (texfile_nm, 'w'), g, m)
	write_deps (open (depfile_nm, 'w'), deps, [texfile_nm, afmfile_nm])


