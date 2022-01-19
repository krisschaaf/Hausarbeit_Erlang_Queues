import os
import glob
import datetime
import psutil
import platform
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import time

class TimeData:
	qty = 0
	time = 0

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
	return os.path.normpath('/Code/CSsystem')

def getNewestZeitReihe():
	# data = getZeitReihen()
	return getZeitReihen()[-1]

def getSecondNewestZeitReihe():
	# data = getZeitReihen()
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
	printSystemInformation()

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
	if type == 1:
			plotAndInterpolateLinLog(data.qty, data.time)
	elif type == 2:
			plotAndInterpolateQuad(data.qty, data.time)
	plt.show()

def plotMulti2D(files=[], lastN=-1, type=1):
	if files == []: files = getLastNZeitReihen(lastN)
	for file in files:
		fileName = chompFileName(file)
		print(fileName)
		data = getDataByName(file)
		if type == 1:
				plotAndInterpolateLinLog(data.qty, data.time, name=fileName)
		elif type == 2:
				plotAndInterpolateQuad(data.qty, data.time, name=fileName)
	plt.show()

def chompFileName(file):
	head, tail = os.path.split(file)
	return str.removesuffix(tail, '.csv')

def plotMulti3D(n):
	data = getLastNZeitReihen(n)
	fig = plt.figure()
	ax = Axes3D(fig)
	plt.show()
	# ax.scatter(data[:, :, 0], data[:, :, 1] )
	# plt.plot(x, y, scheme, label=name)
	# a, b = np.polyfit(x, y, 1)
	# plt.plot(x, a * x + b, label=f"{a:.2E}*x-{-b:.2E}")
	# plt.legend(loc='lower right')
	# plt.xlabel('quantity')
	# plt.ylabel('time/ms')

def plotBothLastInsert():
	dataNew = getNthData(-1, True)
	dataOlder = getNthData(-2, True)
	plotAndInterpolateLinLog(dataOlder.qty, dataOlder.timeInsert/dataOlder.qty, name='older', scheme='b.')
	plotAndInterpolateLinLog(dataNew.qty, dataNew.timeInsert/dataNew.qty, name='newer')

# def getNthData(num, printFiles=False):
# 	file = getNthZeitReihe(num, os.getcwd())
# 	if printFiles: print(num, ': ', file)
# 	dataRaw = np.genfromtxt(file, delimiter=';', comments='Elements')
# 	data = TimeData()
# 	data.qty = dataRaw[2:, 0]
# 	data.timeInsert = dataRaw[2:, 1]
# 	data.timeDelete = dataRaw[2:, 2]
# 	data.timeFind = dataRaw[2:, 3]
# 	data.timeEqual = dataRaw[2:, 4]
# 	data.timeIsBT = dataRaw[2:, 5]
# 	return data

# def evalDataRelation(data1: TimeData, data2: TimeData) -> TimeData:
	# ret = TimeData()
	# ret.qty = data1.qty
	# ret.timeInsert = (np.mean(data1.timeInsert)/np.mean(data2.timeInsert) -1) * 100
	# ret.timeDelete = (np.mean(data1.timeDelete)/np.mean(data2.timeDelete) -1) * 100
	# ret.timeFind = (np.mean(data1.timeFind)/np.mean(data2.timeFind) -1) * 100
	# ret.timeEqual = (np.mean(data1.timeEqual)/np.mean(data2.timeEqual) -1) * 100
	# ret.timeIsBT = (np.mean(data1.timeIsBT)/np.mean(data2.timeIsBT) -1) * 100
	# return ret

# def printDataRelationTotal():
# 	data = evalDataRelation(getNthData(-1), getNthData(-2))
# 	val = (data.timeInsert + data.timeDelete + data.timeFind + data.timeEqual + data.timeIsBT) / 5
# 	printDataRelation(val)

# def printDataRelation(data):
# 	print(f"On average, newer data is {data:.1f} % slower than previous data.")
# def printDataRelationInsert():
# 	data = evalDataRelation(getNthData(-1), getNthData(-2))
# 	printDataRelation(data.timeInsert)
# def printDataRelationDelete():
# 	data = evalDataRelation(getNthData(-1), getNthData(-2))
# 	printDataRelation(data.timeDelete)
# def printDataRelationFind():
# 	data = evalDataRelation(getNthData(-1), getNthData(-2))
# 	printDataRelation(data.timeFind)
# def printDataRelationEqual():
# 	data = evalDataRelation(getNthData(-1), getNthData(-2))
# 	printDataRelation(data.timeEqual)
# def printDataRelationIsBT():
# 	data = evalDataRelation(getNthData(-1), getNthData(-2))
# 	printDataRelation(data.timeIsBT)


# def plotDelete():
# 	data = getNewestData()
# 	plotAndInterpolateLinLog(data.qty, data.timeDelete/data.qty)
# def plotBothLastDelete():
# 	dataNew = getNthData(-1, True)
# 	dataOlder = getNthData(-2, True)
# 	plotAndInterpolateLinLog(dataOlder.qty, dataOlder.timeDelete/dataOlder.qty, name='older', scheme='b.')
# 	plotAndInterpolateLinLog(dataNew.qty, dataNew.timeDelete/dataNew.qty, name='newer')

# def plotFind():
# 	data = getNewestData()
# 	plotAndInterpolateLinLog(data.qty, data.timeFind/data.qty)
# def plotBothLastFind():
# 	dataNew = getNthData(-1, True)
# 	dataOlder = getNthData(-2, True)
# 	plotAndInterpolateLinLog(dataOlder.qty, dataOlder.timeFind/dataOlder.qty, name='older', scheme='b.')
# 	plotAndInterpolateLinLog(dataNew.qty, dataNew.timeFind/dataNew.qty, name='newer')

# def plotEqual():
# 	data = getNewestData()
# 	plotAndInterpolateLin(data.qty, data.timeEqual)
# def plotBothLastEqual():
# 	dataNew = getNthData(-1, True)
# 	dataOlder = getNthData(-2, True)
# 	plotAndInterpolateLin(dataOlder.qty, dataOlder.timeEqual, name='older', scheme='b.')
# 	plotAndInterpolateLin(dataNew.qty, dataNew.timeEqual, name='newer')

# def plotIsBT():
# 	data = getNewestData()
# 	plotAndInterpolateLin(data.qty, data.timeIsBT)
# def plotBothLastIsBT():
# 	dataNew = getNthData(-1, True)
# 	dataOlder = getNthData(-2, True)
# 	plotAndInterpolateLin(dataOlder.qty, dataOlder.timeIsBT, name='older', scheme='b.')
# 	plotAndInterpolateLin(dataNew.qty, dataNew.timeIsBT, name='newer')

def plotAndInterpolateLin(x, y, scheme = 'r.', name=''):
	plt.plot(x, y, scheme, label=name)
	a, b = np.polyfit(x, y, 1)
	plt.plot(x, a * x + b, label=f"{a:.2E}*x-{-b:.2E}")
	plt.legend(loc='lower right')
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

def printSystemInformation():
	print("="*40, "System Information", "="*40)
	uname = platform.uname()
	print(f"System: {uname.system}")
	print(f"Node Name: {uname.node}")
	print(f"Release: {uname.release}")
	print(f"Version: {uname.version}")
	print(f"Machine: {uname.machine}")
	print(f"Processor: {uname.processor}")

	# let's print CPU information
	print("="*40, "CPU Info", "="*40)
	# number of cores
	print("Physical cores:", psutil.cpu_count(logical=False))
	print("Total cores:", psutil.cpu_count(logical=True))
	# CPU frequencies
	cpufreq = psutil.cpu_freq()
	print(f"Max Frequency: {cpufreq.max:.2f}Mhz")
	# print(f"Min Frequency: {cpufreq.min:.2f}Mhz")
	# print(f"Current Frequency: {cpufreq.current:.2f}Mhz")
	# # CPU usage
	# print("CPU Usage Per Core:")
	# for i, percentage in enumerate(psutil.cpu_percent(percpu=True, interval=1)):
	# 	print(f"Core {i}: {percentage}%")
	# print(f"Total CPU Usage: {psutil.cpu_percent()}%")

	# # Memory Information
	# print("="*40, "Memory Information", "="*40)
	# # get the memory details
	# svmem = psutil.virtual_memory()
	# print(f"Total: {get_size(svmem.total)}")
	# print(f"Available: {get_size(svmem.available)}")
	# print(f"Used: {get_size(svmem.used)}")
	# print(f"Percentage: {svmem.percent}%")
	# print("="*20, "SWAP", "="*20)
	# # get the swap memory details (if exists)
	# swap = psutil.swap_memory()
	# print(f"Total: {get_size(swap.total)}")
	# print(f"Free: {get_size(swap.free)}")
	# print(f"Used: {get_size(swap.used)}")
	# print(f"Percentage: {swap.percent}%")

def get_size(bytes, suffix="B"):
    """
    Scale bytes to its proper format
    e.g:
        1253656 => '1.20MB'
        1253656678 => '1.17GB'
    """
    factor = 1024
    for unit in ["", "K", "M", "G", "T", "P"]:
        if bytes < factor:
            return f"{bytes:.2f}{unit}{suffix}"
        bytes /= factor

def progress(total, file):
	# A List of Items
	# Initial call to print 0% progress
	# printProgressBar(0, total, prefix = 'Progress:', suffix = 'Complete', length = 50)
	# fd = open(file)
	num_lines = sum(1 for line in open(file))
	while num_lines <= total:
		num_lines = sum(1 for line in open(file))
		time.sleep(0.1)
		# Update Progress Bar
		printProgressBar(num_lines, total, prefix = 'Progress:', suffix = 'Complete', length = 50)

# Print iterations progress
def printProgressBar (iteration, total, prefix = '', suffix = '', decimals = 1, length = 100, fill = '█', printEnd = "\r"):
    """
    Call in a loop to create terminal progress bar
    @params:
        iteration   - Required  : current iteration (Int)
        total       - Required  : total iterations (Int)
        prefix      - Optional  : prefix string (Str)
        suffix      - Optional  : suffix string (Str)
        decimals    - Optional  : positive number of decimals in percent complete (Int)
        length      - Optional  : character length of bar (Int)
        fill        - Optional  : bar fill character (Str)
        printEnd    - Optional  : end character (e.g. "\r", "\r\n") (Str)
    """
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + '-' * (length - filledLength)
    print(f'\r{prefix} |{bar}| {percent}% {suffix}', end = printEnd)
    # Print New Line on Complete
    if iteration == total: 
        print()

