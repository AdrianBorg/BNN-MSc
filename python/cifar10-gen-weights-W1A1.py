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

import os
from finnthesizer import *

if __name__ == "__main__":
    bnnRoot = "."
    npzFile = bnnRoot + "/cifar10-1w-1a.npz"
    targetDirBin = bnnRoot + "/ParameterFiles"

    weights = []
    threshs = []

    WeightsPrecisions_fractional =    [0 , 0 , 0 , 0 , 0 , 0 , 0, 0,  0]
    ActivationPrecisions_fractional = [0 , 0 , 0 , 0 , 0 , 0 , 0, 0,  0]
    InputPrecisions_fractional =      [7 , 0 , 0 , 0 , 0 , 0 , 0, 0,  0]
    WeightsPrecisions_integer =       [1 , 1 , 1 , 1 , 1 , 1 , 1, 1,  1]
    ActivationPrecisions_integer =    [1 , 1 , 1 , 1 , 1 , 1 , 1, 1,  1]
    InputPrecisions_integer =         [1 , 1 , 1 , 1 , 1 , 1 , 1, 1,  1]

    classes = ['Airplane', 'Automobile', 'Bird', 'Cat', 'Deer', 'Dog', 'Frog', 'Horse', 'Ship', 'Truck']

    #configuration of PE and SIMD counts
    peCounts =    [16, 32, 16, 16,  4,  1, 1, 1, 4]
    simdCounts =  [ 3, 32, 32, 32, 32, 32, 4, 8, 1]

    if not os.path.exists(targetDirBin):
        os.mkdir(targetDirBin)

        #read weights
    rHW = BNNWeightReader(npzFile, True)

    config = "/**\n"
    config+= " * Finnthesizer Config-File Generation\n";
    config+= " *\n **/\n\n"
    config+= "#ifndef __LAYER_CONFIG_H_\n#define __LAYER_CONFIG_H_\n\n"

    # process convolutional layers
    for convl in range(0, 6):
        WPrecision_fractional = WeightsPrecisions_fractional[convl]
        APrecision_fractional = ActivationPrecisions_fractional[convl]
        IPrecision_fractional = InputPrecisions_fractional[convl]
        WPrecision_integer = WeightsPrecisions_integer[convl]
        APrecision_integer = ActivationPrecisions_integer[convl]
        IPrecision_integer = InputPrecisions_integer[convl]
        if convl == 0:
            # use fixed point weights for the first layer
            (w,t) = rHW.readConvBNComplex(WPrecision_fractional, APrecision_fractional, IPrecision_fractional, WPrecision_integer, APrecision_integer, IPrecision_integer, usePopCount=False, numThresBits=24, numThresIntBits=16)
            # compute the padded width and height

        else:
            # regular binarized layer
            (w,t) = rHW.readConvBNComplex(WPrecision_fractional, APrecision_fractional, IPrecision_fractional, WPrecision_integer, APrecision_integer, IPrecision_integer)
        weights.append(w)
        threshs.append(np.asarray(t))
    # process fully-connected layers
    for fcl in range(6,9):
        WPrecision_fractional = WeightsPrecisions_fractional[fcl]
        APrecision_fractional = ActivationPrecisions_fractional[fcl]
        IPrecision_fractional = InputPrecisions_fractional[fcl]
        WPrecision_integer = WeightsPrecisions_integer[fcl]
        APrecision_integer = ActivationPrecisions_integer[fcl]
        IPrecision_integer = InputPrecisions_integer[fcl]

        if fcl == 8:
            (w,t) = rHW.readFCBNComplex_no_thresholds(WPrecision_fractional, APrecision_fractional, IPrecision_fractional, WPrecision_integer, APrecision_integer, IPrecision_integer)
            useThresholds = False
        else:
            (w,t) = rHW.readFCBNComplex(WPrecision_fractional, APrecision_fractional, IPrecision_fractional, WPrecision_integer, APrecision_integer, IPrecision_integer)
            useThresholds = True
        weights.append(w)
        threshs.append(np.asarray(t))
    _w_o = []
    _t_o = []
    for i in range(0, 9):
        w_out = weights[i].astype(dtype='i1')
        _w_o.append(w_out)
        t_out = threshs[i].astype(dtype='i')
        _t_o.append(t_out)

        w_out.tofile(targetDirBin + '/weightsLayer' + str(i))
        t_out.tofile(targetDirBin + '/treshsLayer' + str(i))
    # w0 = weights[0].astype(dtype='>i1')
    # a0 = w0.tobytes()
    # a1 = np.array([[1, 0], [0, 1], [0,1]], dtype='>i1', )
    # s = a1.tobytes(order='F')
    # # for i in range(len(weights)):
    # #     np.array
    # a1.tofile('testt')
    print('end')

