import os
import glob
import datetime
import psutil
import platform
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import time
import pickle

class TimeData:
	qty = 0
	time = 0

# helper und laufzeiten in repo
# call escript bei compilierten dateien
# laufzeiten ding öffnen
# bench.erl zu erlang dateien
# laufzeiten in jupyter notebook öffnen
# benötigte tests einkommentieren
# hbqf.erl 

# Call like erlang:apply but with Args as a string,
# e.g. runErlang('sort', 'qsort', '[left ,[3, 2, 1], 100]')
# or runErlang('bench', 'markFullCombination', '[10, 10, 10, sort, qsort, [a, b, c, d], [4, 5]]')
def runErlang(Mod, Func, Args):
	pathCWD = os.getcwd()
	os.chdir(pathCWD + getBinPath())
	Args = str(Args).replace(" ", "")	# otherwise interpreted as individual arguments
	cmd  = 'escript call.escript ' + str(Mod) + ' ' + str(Func) + ' ' + str(Args)
	ret = os.popen(cmd).read() 
	# ret.replace("\'", "")
	os.chdir(pathCWD)
	if ret == '': ret = 'Something went wrong :('
	return ret


def getBinPath():
	return os.path.normpath('/comp')

def getNewestZeitReihe():
	return getZeitReihen()[-1]

def getSecondNewestZeitReihe():
	return getZeitReihen()[-2]

# -1 is last and so on
def getNthZeitReihe(n):
	return getZeitReihen()[n]

def getLastNZeitReihen(n):
	return getZeitReihen()[-n:]

def getZeitReihen():
	list_of_files = glob.glob(os.getcwd() + getBinPath() + '/*.csv')
	sorted_files = sorted(list_of_files, key=os.path.getmtime)
	return sorted_files

def runBenchmarks(QtyStart, Stepsize, QtySteps, RunsPerMeas, Mode):
	pathCWD = os.getcwd()
	os.chdir(pathCWD + getBinPath())
	# print('Running benchmarks...')
	print(f"zeitBT:messung({QtyStart}, {Stepsize}, {QtySteps}, {RunsPerMeas}, {Mode}).")
	os.popen('escript benchmarks.escript ' + str(QtyStart) + ' ' + str(Stepsize) + ' ' + str(QtySteps) + ' ' + str(RunsPerMeas) + ' ' + Mode).read()
	os.chdir(pathCWD)
	print('Last benchmark finished at ' + str(datetime.datetime.now()))

# -1 is last and so on
def getNthData(n):
	file = getNthZeitReihe(n)
	return getDataByName(file)

def getDataByName(file):
	dataRaw = np.genfromtxt(file, delimiter=';', comments='#')
	data = TimeData()
	data.qty = dataRaw[:, 0]
	data.time = dataRaw[:, 1]
	return data

def getNewestData():
	return getNthData(-1)

def plotSingle2D(file='', type=1):
	if file == '': file = getNewestZeitReihe()
	print(chompFileName(file))
	data = getDataByName(file)
	# figure_handle = plt.figure()
	if type == 1:
			plotAndInterpolateLin(data.qty, data.time)
	elif type == 2:
			plotAndInterpolateQuad(data.qty, data.time)
	plt.show()
	# pl.dump(figure_handle,file('sinus.pickle','w'))

def plotMulti2D(files=[], lastN=-1, type=1):
	if files == []: files = getLastNZeitReihen(lastN)
	figure_handle = plt.figure(figsize=(8,5))
	fileInfos = []
	paramList = ''
	for file in files:	
		fileName = chompFileName(file)
		print(fileName)
		fileInfos = fileName.split('_')
		paramList += '_' + '_'.join(fileInfos[2:])
		data = getDataByName(file)
		if type == 1:
				plotAndInterpolateLin(data.qty, data.time, name=fileName)
		elif type == 2:
				plotAndInterpolateQuad(data.qty, data.time, name=fileName)
		elif type == 5:
				plotAndInterpolateLin(data.qty, data.time / data.qty, name=fileName)
		elif type == 6:
				plotAndInterpolateQuad(data.qty, data.time / data.qty, name=fileName)
	#pickle.dump(figure_handle,open(f"graphen/{fileInfos[0]}_{fileInfos[1]}_{paramList}.pickle",'wb'))
	plt.show()

def chompFileName(file):
	head, tail = os.path.split(file)
	return str.removesuffix(tail, '.csv')

def plotAndInterpolateLin(x, y, scheme = '.', name=''):
	plt.plot(x, y, scheme, label=name)
	a, b = np.polyfit(x, y, 1)
	plt.plot(x, a * x + b, label=f"{a:.2E}*x-{-b:.2E}")
	plt.legend()
	plt.xlabel('quantity')
	plt.ylabel('time/ms')

def plotAndInterpolateLinLog(x, y, scheme = '.', name=''):
	plt.plot(x, y, scheme, label=name)
	a, b = np.polyfit(x * np.log(x), y, 1)
	plt.plot(x, a * x * np.log(x) + b, label=f"{a:.2E}*x*log(x)-{-b:.2E}")
	plt.legend(loc='upper left')
	plt.xlabel('quantity')
	plt.ylabel('time/ms')

def plotAndInterpolateQuad(x, y, scheme = '.', name=''):
	plt.plot(x, y, scheme, label=name)
	a, b = np.polyfit(x * x, y, 1)
	plt.plot(x, a * x * x + b, label=f"{a:.2E}*x^2-{-b:.2E}")
	plt.legend(loc='upper left')
	plt.xlabel('quantity')
	plt.ylabel('time/ms')
