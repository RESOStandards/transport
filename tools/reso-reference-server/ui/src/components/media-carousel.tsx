import { useState } from 'react';

interface MediaCarouselProps {
  /** Array of media records (expanded from $expand=Media). */
  readonly media: ReadonlyArray<Record<string, unknown>>;
  /** Compact mode for summary cards (single thumbnail). */
  readonly compact?: boolean;
}

const PLACEHOLDER_COUNT = 8;

/** Gets the image URL from a media record, falling back to a placeholder. */
const getImageUrl = (record: Record<string, unknown>, index: number): string => {
  if (typeof record.MediaURL === 'string' && record.MediaURL.length > 0) {
    return record.MediaURL;
  }
  return `/images/placeholder-${(index % PLACEHOLDER_COUNT) + 1}.svg`;
};

/** Image carousel for Media records. Compact mode shows a single thumbnail with count. */
export const MediaCarousel = ({ media, compact = false }: MediaCarouselProps) => {
  const [current, setCurrent] = useState(0);

  if (media.length === 0) return null;

  if (compact) {
    return (
      <div className="relative w-full h-32 sm:h-40 rounded overflow-hidden bg-gray-100">
        <img src={getImageUrl(media[0], 0)} alt="Property media" className="w-full h-full object-cover" />
        {media.length > 1 && (
          <span className="absolute bottom-1 right-1 bg-black/60 text-white text-xs px-1.5 py-0.5 rounded">+{media.length - 1}</span>
        )}
      </div>
    );
  }

  return (
    <div className="relative w-full">
      <div className="relative h-48 sm:h-64 md:h-80 rounded-lg overflow-hidden bg-gray-100">
        <img
          src={getImageUrl(media[current], current)}
          alt={`Media ${current + 1} of ${media.length}`}
          className="w-full h-full object-cover"
        />

        {/* Previous button */}
        {media.length > 1 && (
          <button
            type="button"
            onClick={() => setCurrent(prev => (prev > 0 ? prev - 1 : media.length - 1))}
            className="absolute left-2 top-1/2 -translate-y-1/2 bg-black/40 hover:bg-black/60 text-white rounded-full w-8 h-8 flex items-center justify-center"
            aria-label="Previous image">
            &larr;
          </button>
        )}

        {/* Next button */}
        {media.length > 1 && (
          <button
            type="button"
            onClick={() => setCurrent(prev => (prev < media.length - 1 ? prev + 1 : 0))}
            className="absolute right-2 top-1/2 -translate-y-1/2 bg-black/40 hover:bg-black/60 text-white rounded-full w-8 h-8 flex items-center justify-center"
            aria-label="Next image">
            &rarr;
          </button>
        )}

        {/* Counter */}
        <span className="absolute bottom-2 right-2 bg-black/60 text-white text-xs px-2 py-1 rounded">
          {current + 1} / {media.length}
        </span>
      </div>

      {/* Dot indicators */}
      {media.length > 1 && media.length <= 12 && (
        <div className="flex justify-center gap-1.5 mt-2">
          {media.map((m, i) => (
            <button
              type="button"
              key={String(m.MediaKey ?? m.MediaObjectID ?? i)}
              onClick={() => setCurrent(i)}
              className={`w-2 h-2 rounded-full ${i === current ? 'bg-blue-600' : 'bg-gray-300'}`}
              aria-label={`Go to image ${i + 1}`}
            />
          ))}
        </div>
      )}
    </div>
  );
};
