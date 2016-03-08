source('~/tenEasySteps.R')
file = '/home/filip/tenEasySteps.xlsx'
colorfile='/home/filip/tenEasyStepsColors'
directory = '/home/filip/tenEasySteps/'
filename = 'tenEasySteps'

mainTextFrame = createMainTextFrame(file, klatki_per_zdanie = 8)
mainTextFrame = addColors(mainTextFrame, colorfile)
plotTextFrame(mainTextFrame, directory)
makeGif(directory, delay=10, filename)
