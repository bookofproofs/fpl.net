using System.Text;
using System.Collections.Concurrent;

namespace FplLS
{
    public class BufferManager
    {

        public EventHandler<DocumentUpdatedEventArgs> BufferUpdated;

        private ConcurrentDictionary<Uri, StringBuilder> _buffers = new ConcurrentDictionary<Uri, StringBuilder>();

        public void UpdateBuffer(Uri uri, StringBuilder buffer)
        {
            _buffers.AddOrUpdate(uri, buffer, (k, v) => buffer);
            BufferUpdated?.Invoke(this, new DocumentUpdatedEventArgs(uri));
        }

        public StringBuilder GetBuffer(Uri uri)
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
