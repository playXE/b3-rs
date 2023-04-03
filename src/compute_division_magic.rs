/*
 * Copyright (C) 2016 Apple Inc. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * This contains code taken from LLVM's APInt class. That code implements finding the magic
 * numbers for strength-reducing division. The LLVM code on which this code is based was
 * implemented using "Hacker's Delight", Henry S. Warren, Jr., chapter 10.
 *
 * ==============================================================================
 * LLVM Release License
 * ==============================================================================
 * University of Illinois/NCSA
 * Open Source License
 *
 * Copyright (c) 2003-2014 University of Illinois at Urbana-Champaign.
 * All rights reserved.
 *
 * Developed by:
 *
 *     LLVM Team
 *
 *     University of Illinois at Urbana-Champaign
 *
 *     http://llvm.org
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimers.
 *
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimers in the
 *       documentation and/or other materials provided with the distribution.
 *
 *     * Neither the names of the LLVM Team, University of Illinois at
 *       Urbana-Champaign, nor the names of its contributors may be used to
 *       endorse or promote products derived from this Software without specific
 *       prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DivisionMagic {
    pub magic_multiplier: i32,
    pub shift: usize,
}

/// This contains code taken from LLVM's APInt::magic(). It's modestly adapted to our style, but
/// not completely, to make it easier to apply their changes in the future.
pub fn compute_division_magic(divisor: i32) -> DivisionMagic {
    let d = divisor as u32;
    let mut p;
    let ad;
    let anc;
    let mut delta;
    let mut q1;
    let mut r1;
    let mut q2;
    let mut r2;
    let t;

    let signed_min: u32 = i32::MIN as u32;
    let mut mag = DivisionMagic {
        magic_multiplier: 0,
        shift: 0,
    };

    let bit_width = 32;
    ad = if divisor < 0 {
        (-divisor) as u32 
    } else {
        divisor as u32
    };
    
    t = signed_min + (d >> (bit_width - 1));
    anc = t - 1 - t % ad; // absolute value of nc
    p = bit_width - 1; // initialize p
    q1 = signed_min / anc; // initialize q1 = 2p/|nc|
    r1 = signed_min - q1 * anc; // initialize r1 = rem(2p, |nc|)
    q2 = signed_min / ad; // initialize q2 = 2p/|d|
    r2 = signed_min - q2 * ad; // initialize r2 = rem(2p, |d|)

    loop {
        p = p + 1;
        q1 = q1 << 1; // update q1 = 2p/|nc|
        r1 = r1 << 1; // update r1 = rem(2p/|nc|)
        if r1 >= anc { // must be unsigned comparison
            q1 = q1 + 1;
            r1 = r1 - anc;
        }

        q2 = q2 << 1; // update q2 = 2p/|d|
        r2 = r2 << 1; // update r2 = rem(2p/|d|)
        if r2 >= ad { // must be unsigned comparison
            q2 = q2 + 1;
            r2 = r2 - ad;
        }

        delta = ad - r2;

        if q1 < delta || (q1 == delta && r1 == 0) {
            continue;
        } else {
            break;
        }
    }
    
    mag.magic_multiplier = (q2 + 1) as i32;
    if divisor < 0 {
        
        mag.magic_multiplier = -mag.magic_multiplier;
    }

    mag.shift = p - bit_width;
    mag
}
