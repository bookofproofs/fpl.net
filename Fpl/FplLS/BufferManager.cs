using System.Text;
using System.Collections.Concurrent;
using static ErrDiagnostics;

namespace FplLS
{
    /*
    MIT License

    Copyright (c) 2018 Martin Björkström

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.  
    */

    public class BufferManager
    {

        public EventHandler<DocumentUpdatedEventArgs>? BufferUpdated;

        private ConcurrentDictionary<PathEquivalentUri, StringBuilder> _buffers = new ConcurrentDictionary<PathEquivalentUri, StringBuilder>();

        public void UpdateBuffer(PathEquivalentUri uri, StringBuilder buffer)
        {
            _buffers.AddOrUpdate(uri, buffer, (k, v) => buffer);
            BufferUpdated?.Invoke(this, new DocumentUpdatedEventArgs(uri));
        }

        public StringBuilder? GetBuffer(PathEquivalentUri uri)
        {
            return _buffers.TryGetValue(uri, out var buffer) ? buffer : null;
        }

        public class DocumentUpdatedEventArgs : EventArgs
        {
            public PathEquivalentUri Uri { get; }
            public DocumentUpdatedEventArgs(PathEquivalentUri uri)
            {
                Uri = uri;
            }
        }
    }
}
