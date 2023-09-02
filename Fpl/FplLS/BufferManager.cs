using System;
using System.Collections.Concurrent;
using UsingMsLangXml = Microsoft.Language.Xml;

namespace FplLS
{
    public class BufferManager
    {

        public EventHandler<DocumentUpdatedEventArgs> BufferUpdated;

        private ConcurrentDictionary<Uri, UsingMsLangXml.Buffer> _buffers = new ConcurrentDictionary<Uri, UsingMsLangXml.Buffer>();

        public void UpdateBuffer(Uri uri, UsingMsLangXml.Buffer buffer)
        {
            _buffers.AddOrUpdate(uri, buffer, (k, v) => buffer);
            BufferUpdated?.Invoke(this, new DocumentUpdatedEventArgs(uri));
        }

        public UsingMsLangXml.Buffer GetBuffer(Uri uri)
        {
            return _buffers.TryGetValue(uri, out var buffer) ? buffer : null;
        }

        public class DocumentUpdatedEventArgs : EventArgs
        {
            public Uri Uri { get; }
            public DocumentUpdatedEventArgs(Uri uri)
            {
                Uri = uri;
            }
        }
    }
}
