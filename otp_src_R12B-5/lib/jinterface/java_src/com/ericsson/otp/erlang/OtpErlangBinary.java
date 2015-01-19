/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */
package com.ericsson.otp.erlang;

import java.io.Serializable;
import java.io.IOException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

/**
 * Provides a Java representation of Erlang binaries. Anything that
 *  can be represented as a sequence of bytes can be made into an
 *  Erlang binary.
 **/
public class OtpErlangBinary extends OtpErlangBitstr
    implements Serializable, Cloneable
{
    // don't change this!
    static final long serialVersionUID = -3781009633593609217L;
    
    /**
     * Create a binary from a byte array
     *
     * @param bin the array of bytes from which to create the binary.
     **/
    public OtpErlangBinary(byte[] bin) {
	super(bin);
    }
    
    /**
     * Create a binary from a stream containing a binary encoded in
     * Erlang external format.
     *
     * @param buf the stream containing the encoded binary.
     *
     * @exception OtpErlangDecodeException if the buffer does not
     * contain a valid external representation of an Erlang binary.
     **/
    public OtpErlangBinary(OtpInputStream buf) throws OtpErlangDecodeException
    {
	super(new byte[0]);
	this.bin = buf.read_binary();
	this.pad_bits = 0;
    }
    
    /**
     * Create a binary from an arbitrary Java Object. The object must
     * implement java.io.Serializable or java.io.Externalizable.
     *
     * @param o the object to serialize and create this binary from.
     **/
    public OtpErlangBinary(Object o) {
	super(o);
    }



    /**
     * Convert this binary to the equivalent Erlang external representation.
     *
     * @param buf an output stream to which the encoded binary should be
     * written.
     **/
    public void encode(OtpOutputStream buf) {
	buf.write_binary(this.bin);
    }
    
    /**
     * Determine if two binaries are equal. Binaries are equal if they have
     * the same length and the array of bytes is identical.
     *
     * @param o the binary to compare to.
     *
     * @return true if the byte arrays contain the same bytes, false
     * otherwise.
     **/
    public boolean equals(Object o) {
	if (! (o instanceof OtpErlangBinary)) return false;
	
	OtpErlangBinary that = (OtpErlangBinary)o;
	int len = this.bin.length;
	if (len != that.bin.length) return false;
	
	for (int i = 0;  i < len;  i++) {
	    if (this.bin[i] != that.bin[i]) return false; // early exit
	}
	
	return true;
    }
    
    public Object clone() {
	OtpErlangBinary that = (OtpErlangBinary)(super.clone());
	return that;
    }
}
