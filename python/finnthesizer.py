#BSD 3-Clause License
#=======
#
#Copyright (c) 2017, Xilinx
#All rights reserved.
#
#Redistribution and use in source and binary forms, with or without
#modification, are permitted provided that the following conditions are met:
#
#* Redistributions of source code must retain the above copyright notice, this
#  list of conditions and the following disclaimer.
#
#* Redistributions in binary form must reproduce the above copyright notice,
#  this list of conditions and the following disclaimer in the documentation
#  and/or other materials provided with the distribution.
#
#* Neither the name of the copyright holder nor the names of its
#  contributors may be used to endorse or promote products derived from
#  this software without specific prior written permission.
#
#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
#DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
#FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
#CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
#OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# MODIFIED FROM https://github.com/Xilinx/BNN-PYNQ

import numpy as np

def quantize(x, integer, fract):
    bits=integer+fract
    if (bits==1):
        return(binarize(x))
    n = float(2**fract) # GIULIO ADD CLIP
    return np.floor(x * n + 0.5) / n
# the binarization function, basically the sign encoded as 1 for positive and
# 0 for negative
def binarize(w):
    return 1 if w >=0 else 0


# convert a fully connected binarized layer plus batch normalization into 
# the simplified form (binary weight and positive threshold)
# note that the neurons are assumed to be in the columns of the weight
# matrix
def makeFCBNComplex(weights, bias, beta, gamma, mean, invstd, WPrecisions_int=1, WPrecisions_fract=0, use_rowmajor=False, usePopCount=True):
    ins = weights.shape[0]
    outs = weights.shape[1]
    print("Extracting FCBN complex, ins = %d outs = %d",(ins, outs))
    # we'll fill in the binarized weights and thresholds iteratively
    w_bin = list(range(ins*outs))
    w_bin2 = np.zeros((ins, outs))
    thresholds = list(range(outs))
    for neuron in range(outs):
        # compute a preliminary threshold from the batchnorm parameters
        thres = mean[neuron] - bias[neuron] - (beta[neuron] / (gamma[neuron]*invstd[neuron]))
        need_flip = 0
        # ensure all neurons activate on the "positive" side, so we can use
        # greater-than-threshold activation
        if gamma[neuron]*invstd[neuron] < 0:
            need_flip = 1
            thres = -thres
        # turn threshold into "number of 1s" (popcount) instead of signed sum
        if usePopCount:
            thresholds[neuron] = int((ins + thres) / 2)
        else:
            thresholds[neuron] = int(thres)
        # binarize the synapses
        for synapse in range(ins):
            # note how we change from col major to row major if requested
            dest_ind = neuron*ins+synapse if use_rowmajor else synapse*outs+neuron
            if need_flip:
                w_bin[dest_ind] = quantize(-weights[synapse][neuron],WPrecisions_int,WPrecisions_fract)
            else:
                w_bin[dest_ind] = quantize(weights[synapse][neuron],WPrecisions_int,WPrecisions_fract)
            w_bin2[synapse][neuron] = w_bin[dest_ind]
    # reshape the output as desired
    if use_rowmajor:
        w_bin = np.asarray(w_bin).reshape((outs, ins))
    else:
        w_bin = np.asarray(w_bin).reshape((ins, outs))

    return (w_bin, thresholds)

# binarize and pack convolutional layer weights into a matrix and compute
# thresholds from the conv bias and batchnorm parameters
def makeConvBNComplex(weights, bias, beta, gamma, mean, invstd, interleaveChannels, WPrecisions_fract, APrecisions_fract, IPrecisions_fract, WPrecisions_int, APrecisions_int, IPrecisions_int,usePopCount=True,numThresBits=16, numThresIntBits=None):
    WPrecision = WPrecisions_fract + WPrecisions_int
    APrecision = APrecisions_fract + APrecisions_int
    IPrecision = IPrecisions_fract + IPrecisions_int
    numOut = weights.shape[0]
    numIn = weights.shape[1]
    k = weights.shape[2]
    if(k != weights.shape[3]):
        raise Exception("Nonsymmetric conv kernels are not yet supported")
    print("Extracting conv-BN complex, OFM=%d IFM=%d k=%d",(numOut, numIn, k))
    # the fanin is used to ensure positive-only threshold
    fanin = numIn * k * k
    w_bin = list(range(numOut * numIn * k * k))
    w_bin2 = np.zeros((numOut, numIn, k, k))
    # one threshold per output channel
    thresholds = list(range(numOut))
    dest_ind = 0
    step = np.linspace(-1,1,num=2**(APrecision-1),endpoint=False) + 1./(2**(APrecisions_fract+1)) # This one make -0.5 and +0.5 with 2 bits
    # we'll fill in the binarized weights and thresholds iteratively
    for neuron in range(numOut):
        # compute a preliminary threshold from the batchnorm parameters,
        # subtracting the conv bias from the batchnorm mean
        thres = (mean[neuron] - bias[neuron]) - (beta[neuron] / (gamma[neuron]*invstd[neuron]))
        need_flip = 0

        if (numThresIntBits) is None:
            factor = 1
        else:
            factor = 2**(numThresBits - numThresIntBits)

        # ensure all neurons activate on the "positive" side, so we can use
        # greater-than-threshold activation
        if (APrecision==1):
            if gamma[neuron]*invstd[neuron] < 0:
                need_flip = 1
                thres = np.ceil(-factor*thres)
            else:
                thres = np.floor(factor*thres)
            # turn threshold into "number of 1s" (popcount) instead of signed sum
            if usePopCount:
                thresholds[neuron] = int((fanin + thres) / 2)
            else:
                thresholds[neuron] = thres
        else:
            if gamma[neuron]*invstd[neuron] < 0:
                need_flip = 1
                thres = mean[neuron] - bias[neuron] + ((step - beta[neuron]) / (gamma[neuron]*invstd[neuron]))
                thres = np.ceil(-factor*thres)
            else:
                thres = mean[neuron] - bias[neuron] + ((step - beta[neuron]) / (gamma[neuron]*invstd[neuron]))
                thres = np.floor(factor*thres)
            thresholds[neuron] = thres#thres.astype(int)

        for kx in range(k):
            for ky in range(k):
                for ifm in range(numIn):
                    f = -1 if need_flip else +1
                    w_bin[dest_ind] = quantize(f*weights[neuron][ifm][ky][kx], WPrecisions_int, WPrecisions_fract)
                    w_bin2[neuron][ifm][ky][kx] = w_bin[dest_ind]
                    dest_ind += 1

    # reshape the output as desired
    w_bin = np.asarray(w_bin).reshape((numOut, fanin))
    return (w_bin, thresholds)


# pull out data from a numpy archive containing layer parameters
# this should ideally be done using Lasagne, but this is simpler and works
class BNNWeightReader:
    def __init__(self, paramFile, interleaveChannels):
        self.paramDict = np.load(paramFile)
        self.currentParamInd = 0
        self.interleaveChannels = interleaveChannels
        self.numInterleaveChannels = 0

    def __getCurrent(self):
        ret =  self.paramDict["arr_" + str(self.currentParamInd)]
        self.currentParamInd += 1
        return ret

    def readWeightsRaw(self):
        w = self.__getCurrent()
        return w

    def readBatchNormLayerRaw(self):
        bias = self.__getCurrent()
        beta = self.__getCurrent()
        gamma = self.__getCurrent()
        mean = self.__getCurrent()
        invstd = self.__getCurrent()
        return (bias, beta, gamma, mean, invstd)

    # read a fully connected layer plus batchnorm, binarize and convert to
    # positive threshold form, returning (bin weight matrix, thresholds)
    # the returned bin weight matrix has neurons along rows and is suitable
    # to be packed into BNN mems using BNNProcElemMem
    def readFCBNComplex(self, WPrecisions_fract, APrecisions_fract, IPrecisions_fract, WPrecisions_int, APrecisions_int, IPrecisions_int, numThresBits=16, numThresIntBits=None):
        WPrecision = WPrecisions_fract + WPrecisions_int
        APrecision = APrecisions_fract + APrecisions_int
        IPrecision = IPrecisions_fract + IPrecisions_int
        weights = self.readWeightsRaw()
        (bias, beta, gamma, mean, invstd) = self.readBatchNormLayerRaw()

        if WPrecision==1 and APrecision==1 and IPrecision==1:
            (Wb, T) = makeFCBNComplex(weights, bias, beta, gamma, mean, invstd, WPrecisions_int, WPrecisions_fract, use_rowmajor=True)
        elif (APrecision==1):
            (Wb, T) = makeFCBNComplex(weights, bias, beta, gamma, mean, invstd, WPrecisions_int, WPrecisions_fract, use_rowmajor=True, usePopCount=False)
        # else:
        #     (Wb, T) = makeFCBNComplex_QNN(weights, bias, beta, gamma, mean, invstd, WPrecisions_fract, APrecisions_fract, WPrecisions_int, APrecisions_int, True, numThresBits, numThresIntBits)
        # if the interleave flag is set, permute elements in each row
        if self.interleaveChannels and self.numInterleaveChannels != 0:
            print("Interleaving %d channels in fully connected layer...", self.numInterleaveChannels)
            pixPerChan = Wb.shape[1] / self.numInterleaveChannels
            if (APrecisions_fract == 0):
                Wb_perm = np.zeros(Wb.shape, dtype=np.int)
            else:
                Wb_perm = np.zeros(Wb.shape, dtype=np.float)
            for r in range(Wb.shape[0]):
                for chan in range(self.numInterleaveChannels):
                    for cpix in range(int(pixPerChan)):
                        tempp = cpix*self.numInterleaveChannels + chan
                        temp2 = int(chan*pixPerChan + cpix)
                        Wb_perm[r][tempp] = Wb[r][temp2]
            Wb = Wb_perm
            # set interleave to zero once we go past this fc layer
            self.numInterleaveChannels = 0

        return (Wb, T)

        # read a fully connected layer without batchnorm and without using thresholds,
        # returning bin weight matrix
        # the returned bin weight matrix has neurons along rows and is suitable
        # to be packed into BNN mems using BNNProcElemMem
    def readFCBNComplex_no_thresholds(self, WPrecisions_fract, APrecisions_fract, IPrecisions_fract, WPrecisions_int, APrecisions_int, IPrecisions_int, numThresBits=16, numThresIntBits=None):
        WPrecision = WPrecisions_fract + WPrecisions_int
        APrecision = APrecisions_fract + APrecisions_int
        IPrecision = IPrecisions_fract + IPrecisions_int

        weights = self.readWeightsRaw()

        #fake the batchnorm params to use same make functions below
        bias   = np.zeros(weights.shape[1])
        beta   = np.zeros(weights.shape[1])
        gamma  = np.ones(weights.shape[1])
        mean   = np.ones(weights.shape[1])
        invstd = np.ones(weights.shape[1])

        if (WPrecision == 1) and (APrecision == 1) and (IPrecision == 1):
            (Wb, T) = makeFCBNComplex(weights, bias, beta, gamma, mean, invstd, WPrecisions_int, WPrecisions_fract, use_rowmajor=True)
        elif (APrecision==1):
            (Wb, T) = makeFCBNComplex(weights, bias, beta, gamma, mean, invstd, WPrecisions_int, WPrecisions_fract, use_rowmajor=True, usePopCount=False)
        # else:
        #     (Wb, T) = makeFCBNComplex_QNN(weights, bias, beta, gamma, mean, invstd, WPrecisions_fract, APrecisions_fract, WPrecisions_int, APrecisions_int, True, numThresBits, numThresIntBits)

        # if the interleave flag is set, permute elements in each row
        if self.interleaveChannels and self.numInterleaveChannels != 0:
            print ("Interleaving %d channels in fully connected layer..." % self.numInterleaveChannels)
            pixPerChan = Wb.shape[1] / self.numInterleaveChannels
            if (APrecisions_fract == 0):
                Wb_perm = np.zeros(Wb.shape, dtype=np.int)
            else:
                Wb_perm = np.zeros(Wb.shape, dtype=np.float)
            # for r in range(Wb.shape[0]):
            #     for chan in range(self.numInterleaveChannels):
            #         for cpix in range(pixPerChan):
            #             Wb_perm[r][cpix*self.numInterleaveChannels + chan] = Wb[r][chan*pixPerChan + cpix]
            for r in range(Wb.shape[0]):
                for chan in range(self.numInterleaveChannels):
                    for cpix in range(int(pixPerChan)):
                        tempp = cpix*self.numInterleaveChannels + chan
                        temp2 = int(chan*pixPerChan + cpix)
                        Wb_perm[r][tempp] = Wb[r][temp2]
            Wb = Wb_perm
            # set interleave to zero once we go past this fc layer
            self.numInterleaveChannels = 0

        return (Wb, T)

    # read a convolutional layer plus batchnorm, binarize and convert to
    # positive threshold form, returning (bin weight matrix, thresholds)
    # the returned bin weight matrix  is suitable to be packed into BNN mems
    def readConvBNComplex(self, WPrecisions_fract, APrecisions_fract, IPrecisions_fract, WPrecisions_int, APrecisions_int, IPrecisions_int, usePopCount=True,numThresBits=16, numThresIntBits=None):
        weights = self.readWeightsRaw()
        (bias, beta, gamma, mean, invstd) = self.readBatchNormLayerRaw()
        # keep track of output channels for use in FC layer interleave
        self.numInterleaveChannels = weights.shape[0]
        (Wb, T) = makeConvBNComplex(weights, bias, beta, gamma, mean, invstd, self.interleaveChannels, WPrecisions_fract, APrecisions_fract, IPrecisions_fract, WPrecisions_int, APrecisions_int, IPrecisions_int, usePopCount=usePopCount, numThresBits=numThresBits, numThresIntBits=numThresIntBits)
        return (Wb, T)