package net.meltingwax.schemedroid.util;

import net.meltingwax.schemedroid.DebugFlags;
import android.content.Context;
import android.support.v4.content.AsyncTaskLoader;
import android.util.Log;

/**
 * @author Olexandr Tereshchuk - <a href="http://stanfy.com.ua">Stanfy LLC</a>
 * 
 * @param <T>
 *            model type
 */
public abstract class BaseAsyncTaskLoader<T> extends AsyncTaskLoader<T> {

	/** Logging tag. */
	private static final String TAG = "BaseAsyncTaskLoader";
	/** Debug flag. */
	private static final boolean DEBUG = DebugFlags.DEBUG_UTIL;

	/** Result data. */
	private T result;

	/** Is working now. */
	private boolean busy = false;

	public BaseAsyncTaskLoader(final Context ctx) {
		super(ctx);
	}

	@Override
	public void deliverResult(final T data) {
		if (DEBUG) {
			Log.d(TAG, "deliverResult(" + data + ")");
		}
		if (isReset()) {
			return;
		}
		result = data;
		busy = false;
		super.deliverResult(data);
	}

	@Override
	protected void onStartLoading() {
		if (DEBUG) {
			Log.d(TAG, "onStartLoading()");
		}
		if (result != null) {
			deliverResult(result);
		}

		if (takeContentChanged() || result == null) {
			busy = true;
			forceLoad();
		}
	}

	@Override
	protected void onStopLoading() {
		if (DEBUG) {
			Log.d(TAG, "onStopLoading()");
		}
		cancelLoad();
		busy = false;
	}

	@Override
	protected void onReset() {
		if (DEBUG) {
			Log.d(TAG, "onReset()");
		}
		onStopLoading();
		result = null;
	}

	@Override
	public void onCanceled(final T data) {
		if (DEBUG) {
			Log.d(TAG, "onCanceled(" + data + ")");
		}
		super.onCanceled(data);
		busy = false;
	}

	@Override
	protected void onForceLoad() {
		if (DEBUG) {
			Log.d(TAG, "onForceLoad()");
		}
		super.onForceLoad();
		busy = true;
	}

	public boolean isBusy() {
		return busy;
	}

}
